Zwaluw is a Haskell combinator library for specifying URL routers. The routers
are bidirectional: they parse a URL into a sitemap and serialise sitemap
values back to URLs. Because you specify such a router only once, the
resulting parser and serialiser are each other's inverse *by construction*.

Zwaluw derives pure routers for all your constructors. You can either use pure
Template Haskell to derive these routers (see [ExampleTH.hs][exampleTH]), or
use the regular package (see [ExampleRegular.hs][exampleRegular]). With those
constructor routers, you can build your own router, combining them and
producing/consuming URLs. Both files contain an example of this.

[exampleTH]: https://github.com/MedeaMelana/Zwaluw/blob/master/ExampleTH.hs
[exampleRegular]: https://github.com/MedeaMelana/Zwaluw/blob/master/ExampleRegular.hs
