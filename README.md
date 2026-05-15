# NMDA receptor ablation in medial prefrontal cortex disrupts value updating and reward history integration

This repository contains the basic data and analysis code needed to reproduce the primary findings reported in the manuscript.

**Authors:** Evan Knep<sup>1</sup>, Angelica Velosa<sup>1</sup>, Dana Mueller<sup>1</sup>, Cathy Chen<sup>2</sup>, Sophia Vinogradov<sup>2</sup>, Matthew V. Chafee<sup>3</sup>, Becket Ebitz<sup>4</sup>, Sarah Heilbronner<sup>5</sup>, Patrick E. Rothwell<sup>3</sup>, Nicola Grissom<sup>1</sup>

**Affiliations**

<sup>1</sup> Department of Psychology, University of Minnesota, Minneapolis, Minnesota  
<sup>2</sup> Department of Psychiatry, University of Minnesota, Minneapolis, Minnesota  
<sup>3</sup> Department of Neuroscience, University of Minnesota, Minneapolis, Minnesota  
<sup>4</sup> Department of Neurosciences, Université de Montréal, Québec, Canada  
<sup>5</sup> Department of Neurosurgery, Baylor College of Medicine, Houston, Texas


## Repository contents

This repository contains the core data and R functions needed to reproduce the main behavioral, reinforcement-learning, and simulation-based analyses reported in the manuscript.

### `R/00_paper_functions.R`

Core R functions used for manuscript analyses. This file includes functions for:

- computing behavioral summary measures from trial-level data
- calculating reward acquisition relative to chance
- calculating optimal choice rate, switching, win-stay, lose-shift, negative outcome weight, mutual information, richness, and Δprobability
- fitting and evaluating the additive-gains RLCK model
- generating RLCK-based simulations and posterior predictive checks

The public version focuses on the final additive-gains RLCK model used in the manuscript rather than including all exploratory model-comparison code.

### `data/trial_by_trial.csv.zip`

Compressed trial-level behavioral dataset used for the main analyses.

Each row corresponds to a single trial from the restless bandit task. The included columns are:

| Column | Description |
|---|---|
| `animal` | Animal identifier |
| `virus` | Viral manipulation group, typically `lacz` control or `grin1` ablation |
| `sex` | Biological sex of the animal |
| `surgery` | Pre- versus post-surgery phase |
| `weeks_post_surgery` | Week relative to surgery |
| `condition` | Experimental/pharmacological condition, such as no injection, saline, or MK-801 |
| `schedule` | Task schedule/session identifier |
| `choice` | Animal’s choice on the trial, coded by side/option |
| `outcome` | Trial outcome, coded as rewarded or unrewarded |
| `left_prob` | Reward probability assigned to the left option on that trial |
| `right_prob` | Reward probability assigned to the right option on that trial |

The reward probabilities define the restless bandit environment and are used to compute chance reward rate, optimal choice rate, richness, and Δprobability.
