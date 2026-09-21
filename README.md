# Multiple Knapsack Problem (MKP)

A **Mixed Integer Linear Programming (MILP)** model in **R** for the **Multiple Knapsack Problem**, built with the [`ompr`](https://dirkschumacher.github.io/ompr/) modeling framework and solved via the **SYMPHONY** solver (through `ROI`).

## Overview

The Multiple Knapsack Problem (MKP) is a classic combinatorial optimization problem in Operations Research. Given a set of items, each with a known weight and value, and a set of knapsacks, each with a fixed capacity, the goal is to **select which items to assign to which knapsack** so as to **maximize the total value** of selected items — without exceeding any knapsack's capacity.

Unlike the Bin Packing Problem, the MKP has a fixed number of knapsacks and items may be **left unassigned** when they cannot all fit. Unlike the standard single-knapsack problem, items must be distributed across **multiple knapsacks** with independent capacities. The MKP is NP-hard and has applications in resource allocation, cargo loading, budget planning, and project selection.

## Repository Contents

| File | Description |
|---|---|
| `Multiple Knapsack.R` | R script implementing and solving the MKP instance |
| `Multiple_Knapsack_Problem.pdf` | Mathematical formulation of the problem |

## Mathematical Formulation

### Parameters

- $n$ = number of items (index $i = 1, \dots, n$)
- $m$ = number of knapsacks (index $j = 1, \dots, m$)
- $w_i$ = weight of item $i$; $\forall\, i = 1, \dots, n$
- $v_i$ = value of item $i$; $\forall\, i = 1, \dots, n$
- $C_j$ = capacity of knapsack $j$; $\forall\, j = 1, \dots, m$

### Variable

- $x_{ij}$ = binary assignment variable:

$$
x_{ij} = \begin{cases} 1 & \text{if item } i \text{ is placed in knapsack } j \\ 0 & \text{otherwise} \end{cases}
$$

### Objective Function

**(1)** — Maximize total value of selected items

$$
\displaystyle \max \sum_{i=1}^{n} \sum_{j=1}^{m} v_i \cdot x_{ij}
$$

### Constraints

**(2)** — Capacity: the total weight of items assigned to each knapsack cannot exceed its capacity

$$
\displaystyle \sum_{i=1}^{n} w_i \cdot x_{ij} \le C_j \qquad \forall\, j = 1, \dots, m
$$

**(3)** — Each item can be assigned to at most one knapsack

$$
\displaystyle \sum_{j=1}^{m} x_{ij} \le 1 \qquad \forall\, i = 1, \dots, n
$$

**(4)** — Binary assignment variables

$$
x_{ij} \in \{0, 1\} \qquad \forall\, i = 1, \dots, n,\ j = 1, \dots, m
$$

> **Note on constraint (3):** The inequality $\le 1$ (rather than $= 1$) allows items to be left unassigned when there is insufficient capacity to include them. This distinguishes the MKP from the Bin Packing Problem, where every item must be packed. Here, the model **selects** the most valuable subset of items that can be feasibly distributed across the available knapsacks.

A copy of this formulation is also available as a standalone PDF in this repository.

## Example Instance

The script uses a hardcoded instance with **15 items** and **5 knapsacks**, each with capacity $C_j = 100$:

| Item $i$ | Weight $w_i$ | Value $v_i$ |
|:---:|---:|---:|
| 1 | 48 | 10 |
| 2 | 30 | 30 |
| 3 | 42 | 25 |
| 4 | 36 | 50 |
| 5 | 36 | 35 |
| 6 | 48 | 30 |
| 7 | 42 | 15 |
| 8 | 42 | 40 |
| 9 | 36 | 30 |
| 10 | 24 | 35 |
| 11 | 30 | 45 |
| 12 | 30 | 10 |
| 13 | 42 | 20 |
| 14 | 36 | 30 |
| 15 | 36 | 25 |
| **Total** | **558** | **430** |

- **Knapsack capacity**: $C_j = 100$ for all $j = 1, \dots, 5$
- **Total capacity**: $5 \times 100 = 500$ units
- **Total item weight**: $558$ units — exceeds total capacity by **58 units**, so not all items can be packed and the model must select the most valuable feasible subset

## Requirements

```r
install.packages(c("lpSolve", "dplyr", "ROI", "ROI.plugin.symphony", "ompr", "ompr.roi"))
```

## Usage

1. Clone or download this repository.
2. Open `Multiple Knapsack.R` in R or RStudio.
3. Update the `setwd()` path at the top of the script to match your local directory.
4. Run the script. It will:
   - Build and solve the MILP model using `ompr` and SYMPHONY
   - Print the model status and optimal total value (objective)
   - Print all active assignment variables $x[i, j] = 1$, showing which item is placed in which knapsack

## Output

The script prints:

- **Model status** — whether an optimal solution was found
- **Objective value** — the maximum total value achievable
- **$x[i, j]$ variables** — all item-to-knapsack assignments in the optimal solution (only non-zero values)
