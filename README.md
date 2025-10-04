# Derivative Pricer in OCaml

A Monte Carlo simulation-based pricer for financial derivatives (Vanilla, Asian, and American options) using the Black-Scholes model. Implemented in OCaml with Owl for numerical computations.

## Features

- Supports European (Vanilla), Asian, and American options.
- Monte Carlo pricing with confidence intervals.
- Delta calculation via finite differences.
- Modular design with separate [`model`](model.ml) and [`pricer`](pricer.ml) modules.

## Prerequisites

- OCaml 5.2+
- OPAM
- Dune
- Docker (for containerized runs)

## Local Installation and Run

1. Install dependencies:
   ```bash
   opam install owl base stdio
    ```
2. Build the project:
   ```bash
   dune build
    ```
3. Run the pricer:
   ```bash
   ./_build/default/model.exe
    ```
    This simulates pricing an American call option (strike 100, spot 100, r=0.05, sigma=0.2, maturity=1.0) with 1000 simulations and 100 steps, outputting price, delta, and time taken.s

## Run with Docker

Prerequisites: Docker installed.
```bash
### 1. Build the image
docker build -t deriv-pricer .

### 2. Run the pricer
docker run --rm deriv-pricer

### 3. Rebuild after code changes
docker build -t deriv-pricer .
```

## Usage
Modify [`model.ml`](model.ml) to change parameters or contracts. The main function demonstrates pricing an American call option.

## License
MIT License - see [`LICENSE`](LICENSE) for details.