// partA.mod - Deterministic RBC model with temporary technology shock

// 1. Declare variables
var y c k i l y_l w r z;
varexo e;

// 2. Declare parameters
parameters beta psi delta alpha rho sigma epsilon;
alpha = 0.33;
beta = 0.99;
delta = 0.025;
psi = 1.75;
rho = 0.95;
sigma = (0.007 / (1 - alpha));
epsilon = 10;

// 3. Model equations
model;
  // Euler equation
  (1/c) = beta * (1/c(+1)) * (1 + r(+1) - delta);
  
  // Intratemporal labor-leisure condition
  psi * c / (1 - l) = w;
  
  // Resource constraint
  c + i = y;

  // Production function
  y = (k(-1)^alpha) * (exp(z) * l)^(1 - alpha);

  // Factor prices
  w = y * ((epsilon - 1)/epsilon) * (1 - alpha) / l;
  r = y * ((epsilon - 1)/epsilon) * alpha / k(-1);

  // Capital accumulation
  i = k - (1 - delta) * k(-1);

  // Output per labor (optional)
  y_l = y / l;

  // TFP shock process
  z = rho * z(-1) + e;
end;

// 4. Initial values (steady state guess)
initval;
  k = 9;
  c = 0.76;
  l = 0.3;
  w = 2.07;
  r = 0.03;
  z = 0;
  e = 0;
end;

// ← finds the steady state
steady;      

// 4 b. Terminal steady state after the shock has died out
endval;
  z = 0;     // productivity back to normal
end;

// 5. Temporary technology shock in periods 1–5
shocks;
  var e;
  periods 1 2 3 4 5;
  values 0.1 0.1 0.1 0.1 0.1;
end;

perfect_foresight_setup(periods=200);
perfect_foresight_solver;
