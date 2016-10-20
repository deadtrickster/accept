@copyright 2016 Ilya Khaprov <<i.khaprov@gmail.com>>.
@title Accept header(s) for Erlang/Elixir
@version 0.1.0

@doc

[![Hex.pm](https://img.shields.io/hexpm/v/accept.svg?maxAge=2592000?style=plastic)](https://hex.pm/packages/accept)
[![Hex.pm](https://img.shields.io/hexpm/dt/accept.svg?maxAge=2592000)](https://hex.pm/packages/accept)
[![Build Status](https://travis-ci.org/deadtrickster/accept.svg?branch=version-3)](https://travis-ci.org/deadtrickster/accept)
[![Coverage Status](https://coveralls.io/repos/github/deadtrickster/accept/badge.svg?branch=master)](https://coveralls.io/github/deadtrickster/accept?branch=master)

## Examples

### Accept Header

#### Parsing

<pre lang="erlang">
1> accept_header:parse("text/*;q=0.3, text/html;q=0.7, text/html;level=1,"
                       "text/html;level=2;q=0.4, */*;q=0.5").
[{media_range,"text","*",0.3,[]},
 {media_range,"text","html",0.7,[]},
 {media_range,"text","html",1,[{"level","1"}]},
 {media_range,"text","html",0.4,[{"level","2"}]},
 {media_range,"*","*",0.5,[]}]
</pre>

#### Content Negotiation

<pre lang="erlang">
2> accept_header:negotiate("text/*;q=0.3, text/html;q=0.7, text/html;level=1,"
                           "text/html;level=2;q=0.4, */*;q=0.5",
                           ["text/html;level=2", "text/html;level-3"]).
"text/html;level-3"
</pre>

`"text/html;level-3"' returned because `"text/html;level=2"' matches to
`text/html;level=2;q=0.4' with score 0.4 and most specific match for
`"text/html;level-3"' is `text/html;q=0.7' with score 0.7.

<pre lang="erlang">
3> accept_header:negotiate("application/xml,application/xhtml+xml,"
3>                         "text/html;q=0.9,text/plain;q=0.8,image/png,image/*;q=0.9,*/*;q=0.5",
3>                         ["text/n3",
3>                          "application/rdf+xml"]).
"text/n3"
</pre>

Negotiate preserves user-defined order for equally scored alternatives.

## Contributing

Sections order:

`Types -> Macros -> Callbacks -> Public API -> Deprecations -> Private Parts'

install git precommit hook:

```./bin/pre-commit.sh install'''

 Pre-commit check can be skipped passing `--no-verify' option to git commit.

## License

MIT

@end


