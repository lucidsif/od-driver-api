Driver-api
========

`GraphQL` applicationvusing [morpheus](https://morpheusgraphql.com/) and [hasbolt](https://hackage.haskell.org/package/hasbolt), respectively.

Inspiration for this project came to replace the graphql API mounted on the rails server in `so-api` that is used by the drive app built with Swift.
In the spirits of microservice architecture purity and the natural affinity of Haskell and Graphql for one and another, I built this prototype that 
is probably far from compiling. The major issue aside from being a MVP of a production graphql API using Haskell
is that I am reinventing the wheel by writing an ORM like system that we got for free. I am focused on learning Haskell and a production ready Graphql
server, both of which are not trivial on its own and certainly not with real life things (at least for someone like me). Writing raw SQL strings and
reinventing an ORM in haskell for a graphql is not quite on my bucket list. This is where someone with probably even a modicm of more experience 
than I have can guide me towards integrating an ORM like system similar to Ruby on Rails/

I do not think this should be committed to production, primarily that I do not think reinventing a simple ORM in Haskell is not quite
approrpriate for a project that should be easily read, extended, and be maintainable by a team. In addition, to kill two birds with one stone, 
I also saw this project as a learning opportunity as I am very interested in GraphQL and a a strongly or statically styped functional programming
language. I have found the motivation I needed to learn Haskell. I certainly think building one that meets the characteristics mentioned earlier 
is feasible since major roadblock is that this haskell graphql api is missing an ORM like system.

I humbly present you my terrible attempt at building a graphql API using Haskell that is surely riddled with mediocrity that took longer than I liked , 
but we all have to start somewhere I suppose. If you are a Haskell master, please guide me, senpai and show me what I should or 
could have been doing. My ego was dissolved when I came across monads - I am here to learn and join the Haskell ranks if I am deserving. Please let this project
help you determine that.

### GraphQL API via Http

Go to `http://localhost:3000/api` and start sending queries. 

You can also get the GraphQL Schema at `http://localhost:3000/schema.gql`.

### Queries

### Mutations

