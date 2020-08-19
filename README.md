Driver-api
========

[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

`GraphQL` applicationvusing [morpheus](https://morpheusgraphql.com/) and [hasbolt](https://hackage.haskell.org/package/hasbolt), respectively.


### Run it locally

##### Dependencies

Docker

```
docker run -it --rm -p7474:7474 -p7687:7687 --env NEO4J_AUTH=neo4j/test neo4j:latest
```

##### Using Cabal

```
cabal new-run musikell
```

##### Using Nix

Does not require to have `cabal` installed.

```
nix-shell --pure shell.nix
cabal new-run musikell
```

### GraphQL API via Http

Go to `http://localhost:3000/api` and start sending queries. Eg. using `Insomnia`:

![insomnia](insomnia.png)

You can also get the GraphQL Schema at `http://localhost:3000/schema.gql`.

### Queries

### Mutations

