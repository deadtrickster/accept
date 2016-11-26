

# Module accept_header #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

HTTP Accept header parser and content-type negotiation helper.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#negotiate-2">negotiate/2</a></td><td>
Negotiates the most appropriate content_type given the accept header
and a list of alternatives.</td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td>
Parses Accept header, returns a list of media_ranges.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="negotiate-2"></a>

### negotiate/2 ###

<pre><code>
negotiate(Header, Alternatives) -&gt; Match
</code></pre>

<ul class="definitions"><li><code>Header = BinaryOrString</code></li><li><code>Alternatives = [Alternative]</code></li><li><code>Alternative = BinaryOrString | {BinaryOrString | Tag}</code></li><li><code>BinaryOrString = binary() | string()</code></li><li><code>Tag = any()</code></li><li><code>Match = Tag | nomatch</code></li></ul>

Negotiates the most appropriate content_type given the accept header
and a list of alternatives.

```erlang-repl

  accept_header:negotiate("text/*;q=0.3, text/html;q=0.7, text/html;level=1,"
                          "text/html;level=2;q=0.4, */*;q=0.5",
                          ["text/html;level=2", "text/html;level-3"]).
  "text/html;level-3"
```

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(AcceptString) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>AcceptString = binary() | string()</code></li><li><code>Result = [<a href="#type-media_range">media_range()</a>]</code></li></ul>

Parses Accept header, returns a list of media_ranges.

```erlang-repl

  accept_header:parse("text/*;q=0.3, text/html;q=0.7, text/html;level=1,"
                      "text/html;level=2;q=0.4, */*;q=0.5").
  [{media_range,"text","*",0.3,[]},
   {media_range,"text","html",0.7,[]},
   {media_range,"text","html",1,[{"level","1"}]},
   {media_range,"text","html",0.4,[{"level","2"}]},
   {media_range,"*","*",0.5,[]}]
```

