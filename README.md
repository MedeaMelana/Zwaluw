Zwaluw is a Haskell combinator library for specifying URL routers. The routers
are bidirectional: they parse a URL into a sitemap and serialise sitemap
values back to URLs. Because you specify such a router only once, the
resulting parser and serialiser are each other's inverse *by construction*.
See [Example.hs][example] for an example router.

[example]: https://github.com/MedeaMelana/Zwaluw/blob/master/Example.hs
