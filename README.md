# crit-rate-damage-optimisation

In certain gamemodes, specifically speedruns, the goal is to deal maximum damage in the shortest time. Two key stats often affect this:
Crit Rate (probability of a critical hit)
Crit Damage (magnitude of the critical hit)

These stats have a trade-off: increasing one decreases the other.

But here’s the twist: you're allowed multiple attempts and can take only the best result.

So the question becomes:
What is the optimal Crit Rate and Crit Damage allocation to maximise your best damage output across multiple tries?

What the Code Considers
The number of hits per attempt and how much each hit contributes (weighting)
The number of retries you're willing to make to get a better result
The total stat pool available for allocation
The trade-off function between Crit Rate and Crit Damage
Simulations via Monte Carlo methods to model thousands of combat scenarios

Real-World Analogy
Imagine you're spinning multiple wheels to win prizes. Each wheel:
Has prizes with different sizes (some large, some small)
Lets you trade off the chance of winning for bigger prizes
Can be spun multiple times, and you keep only the best result
How much probability should you sacrifice to increase prize size, if your goal is to maximize the best prize across limited tries?

This project answers that.
