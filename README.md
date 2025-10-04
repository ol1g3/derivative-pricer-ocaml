# derivative-pricer-ocaml

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