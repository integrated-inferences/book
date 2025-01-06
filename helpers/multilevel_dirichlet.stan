data {

	int<lower=2> n_studies;
	int<lower=1> n_params;
	int<lower=1> n_types;
	int<lower=1> n_param_sets;
	int<lower=1> n_param_each[n_param_sets];
	int<lower=1> n_data;
	int<lower=1> n_events;
	int<lower=1> n_strategies;

	int<lower=1> l_starts[n_param_sets];
	int<lower=1> l_ends[n_param_sets];
	int<lower=1> strategy_starts[n_strategies];
	int<lower=1> strategy_ends[n_strategies];

	vector[n_types] P[n_params] ;
	vector[n_types] not_P[n_params] ;
	matrix<lower=0,upper=1>[n_types, n_data] A;
	matrix<lower=0,upper=1>[n_events,n_data] E;
	int<lower=0> Y[n_events, n_studies];

}


parameters {
	vector<lower=.1>[n_params] alpha;
	matrix<lower=0>[n_params - n_param_sets, n_studies] gamma;
	}


transformed parameters {
	matrix<lower=0>[n_params, n_studies] lambdas;
	matrix<lower=1>[n_param_sets, n_studies] sum_gammas;

	for(k in 1:n_studies){
		for (i in 1:n_param_sets) {

		sum_gammas[i, k] = 1 + sum(gamma[(l_starts[i] - (i-1)):(l_ends[i] - i), k]);

		lambdas[l_starts[i]:l_ends[i], k] =
		append_row(1, gamma[(l_starts[i] - (i-1)):(l_ends[i] - i), k]) / sum_gammas[i, k];

		}
	}
}


model {
	vector[n_data] w;
	vector[n_events] w_full;
	vector[n_types]  prob_of_types;
	vector[n_params] P_lambdas[n_types];
    
  target += inv_gamma_lpdf(alpha | 1, 1);
    
    for(k in 1: n_studies){
		for (i in 1:n_types) {
		  for (j in 1:n_params) {
			P_lambdas[i, j] = P[j, i] .* lambdas[j,k] + not_P[j, i];
		  }
		  prob_of_types[i] = prod(P_lambdas[i]);
		}

		w = A' * prob_of_types;
		w_full = E * w;

		for (i in 1:n_param_sets) {
		  target += dirichlet_lpdf(lambdas[l_starts[i]:l_ends[i],k]  | alpha[l_starts[i] :l_ends[i]]);
		  target += -n_param_each[i] * log(sum_gammas[i, k]);
		 }

		for (i in 1:n_strategies) {
		  target += multinomial_lpmf(
		  Y[strategy_starts[i]:strategy_ends[i],k] | w_full[strategy_starts[i]:strategy_ends[i]]);
		 }
	 }

}
