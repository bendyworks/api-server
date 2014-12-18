# API Server

An API Server in Haskell. Written as a demonstration of how to use
[hasql][hsql] and [scotty][stty] together in a realistic (we hope) api
server application.

[hsql]: http://hackage.haskell.org/package/hasql
[stty]: http://hackage.haskell.org/package/scotty

There's a `User` and a minimal `Resource` (which can easily be extended
or copied). Even these two resources interact in a nice way to
demonstrate things like required or optional parameters and token-based
authentication.


## Setup

This application is "cabalized" and so it should be the same as running any
other cabal-managed application. Make sure that you have [cabal][cabl] and
cabal-install installed and working. For the majority of users, installing
the [Haskell Platform][hplt] will be all you need.

[cabl]: https://www.haskell.org/cabal/
[hplt]: https://www.haskell.org/platform/

Next clone the repo and then initialize a new project. This project uses
[cabal freeze][frze] to lock-in the versions of the necessary dependencies.

[frze]: http://blog.johantibell.com/2014/04/announcing-cabal-120.html#dependency-freezing

    git clone https://github.com/bendyworks/api_server.git
    cd api_server

It is good practice to create a sandbox for installing packages into:

    cabal sandbox init

Next install the project's dependencies. We'll also install the
dependencies for tests:

    cabal install --only-dependencies --enable-tests

At this point, go get a snack, it's going to build for a while. Next we
need to initialize the database that the app depends upon. There's a script
which will set up the database (you can also call `db drop` to drop the
databases) This will create `api_dev` and `api_test` databases using the
[psql][] command:

[psql]: http://www.postgresql.org/docs/current/static/app-psql.html

    ./bin/db create

Lastly, we'll set some environment variables. I usually create a `.env`
file with some details like this:

    export DATABASE_URL="postgresql://postgres:@localhost:5432/api_dev"
    export SMTP_SERVER=localhost
    export SMTP_PORT=25
    export SMTP_LOGIN=""
    export SMTP_PASSWORD=""
    export HASK_ENV=DEVELOPMENT
    export PORT=3000

⚠️ **Note**: The same environment variables will need to be set in production.

Just to make sure everything worked, we'll build the tests and run them:

    cabal test --show-details=always --test-options="--color"

If you look in the cabal file (`api-server.cabal`) you can see that the
project is split into three pieces:

* _A library_ that contains the vast majority of the application logic.
* _An executable_ that uses the library and is basically there to gather
  some environment variables to run the server.
* _A test suite_ that uses the library.

Undoubtedly, I will have missed something in these docs. Please file an
issue and we'll get the setup corrected.

## API Flow

The basic flow for the api is as follows:

    REQ> POST /users

    RSP> user_id: x, api_token: t

    REQ> POST /users/$x
         { "resource_email": "email@example.com" }

    RSP> "ok" (also sends an email to given address, with uuid)

    REQ> GET /verify/$uuid

    RSP> {resource_id: r, user_id: x}

    REQ> POST /resource
         Header: "Authorization: Token $t"
         { "resource_email": "email@example.com"
         , "resource_name": "some name"
         , "resource_optional": "optional text"
         , "user_id": $x
         }

    RSP> { "resource_email": "email@example.com"
         , "resource_name": "some name"
         , "resource_optional": "optional text"
         , "resource_id": $r
         }

## Other documentation

There's a series of blog posts that describes how we built this application
and the kinds of things that we were hoping to achieve:

* [Part 1][prt1] - db concerns
* [Part 2][prt2] - encoding domain logic in types
* [Part 3][prt3] - type system as an aid to authentication

[prt1]: http://bendyworks.com/actually-using-the-database/
[prt2]: https://bendyworks.com/haskell-api-server-2/
[prt3]: https://bendyworks.com/authentication-via-haskell/

## Thanks

* Bendyworks for professional development time & the room to try new stuff
* Jon for putting in all the time that actually makes a project like this
  work. He’s always been the one that fixes the half-baked stuff that I
  write. Also, it has been immensely rewarding to pair-program on Haskell
  professionally.
* All of the various excellent Haskell libraries and tooling.
* And last, but certainly not least, Joe Nelson for his utterly
  indefatigable work on everything Haskell, all of which I depend on
  constantly: [haskell-vim-now][hvin], [heroku-buildpack-ghc][hbpg],
  [postgrest][rest] and [more][].  Also I want to thank Joe for hours of
  deep conversations during [BayHac2014][byhc]  about what’s possible using
  databases, Haskell, and so much more. My understanding about how to
  design applications like this wouldn’t have been possible without his
  insights.  His urging (even as a voice in the back of my head) has caused
  me to finish more projects than anything else. Thanks Joe!

[hvin]: https://github.com/begriffs/haskell-vim-now
[hbpg]: https://github.com/begriffs/heroku-buildpack-ghc
[rest]: https://github.com/begriffs/postgrest
[more]: https://github.com/begriffs?tab=repositories
[byhc]: https://www.haskell.org/haskellwiki/BayHac2014
