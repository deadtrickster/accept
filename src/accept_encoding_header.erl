-module(accept_encoding_header).

-export([parse/1,
         negotiate/2]).

-include("accept.hrl").

%%====================================================================
%% Public API
%%====================================================================

-spec parse(AcceptString) -> Result when
    AcceptString :: binary() | string (),
    Result :: [content_coding()].
parse(AcceptString) ->
  accept_parser:map_options(fun parse_content_coding/1, AcceptString).

negotiate(undefined, Alternatives) ->
  case Alternatives of
    [H|_] -> H;
    _ -> Alternatives
  end;
negotiate([], _) ->
  identity;
negotiate(Header, Alternatives) ->
  CCs = parse(Header),

  Alts = lists:map(fun (Alt) ->
                       {A, Tag} = case Alt of
                                    {_, _} -> Alt;
                                    _ -> {Alt, Alt}
                                  end,
                       PA = parse_content_coding(accept_parser:ensure_string(A)),
                       %% list of Alt-CC scores
                       AltCCScores = lists:map(fun (CC) ->
                                                   {score_alt(CC, PA), CC}
                                               end,
                                               CCs),
                       %% best Content Coding match for this Alternative
                       [{Score, BCC} | _ ] = lists:sort(fun scored_cmp/2,
                                                        AltCCScores),
                       case Score of
                         0 ->
                           {-1, Tag};
                         _ ->
                           #content_coding{q = BCCQ} = BCC,
                           {BCCQ, Tag}
                       end
                   end,
                   Alternatives),

  {Q, Tag} = find_preferred_best(lists:keysort(1, Alts)),
  case Q of
    Q when Q =< 0 -> fallback_to_identity(CCs);
    _ -> Tag
  end.

%%====================================================================
%% Private Parts
%%====================================================================

parse_content_coding(#accept_option{option=Coding,
                                    q=Q,
                                    params=Params}) ->
  %% RFC [https://tools.ietf.org/html/rfc7230#section-4] says
  %% that params are allowed only for 'not standard' coding
  %% this can be good TODO item
  #content_coding{coding = Coding,
                  q = Q,
                  params = Params};
parse_content_coding(String) ->
  parse_content_coding(accept_parser:parse_option(String)).

fallback_to_identity(CCs) ->
  %% "identity is always valid
  %% unless idnetity; q=0 or *;q=0
  identity_fallback(CCs, ["identity", "*"]).


identity_fallback(_, []) ->
  identity;
identity_fallback(CCs, [C|R]) ->
  case lists:keyfind(C, #content_coding.coding, CCs) of
    #content_coding{q = 0} ->
      undefined;
    #content_coding{} ->
      identity;
    false ->
      identity_fallback(CCs, R)
  end.

scored_cmp({S1, _}, {S2, _}) ->
  S1 > S2.

find_preferred_best(Sorted) ->
  [B | R] = lists:reverse(Sorted),
  find_preferred_best(B, R).

find_preferred_best({Q, _}, [{Q, _} = H | R]) ->
  find_preferred_best(H, R);
find_preferred_best(B, []) ->
  B;
find_preferred_best(B, _) ->
  B.

score_alt(#content_coding{coding = Coding,
                          params = CCParams},
          #content_coding{coding = Coding,
                          params = AltParams}) ->
  4 + score_params(CCParams, AltParams);
score_alt(#content_coding{coding = "*"},
          _) ->
  4;
score_alt(_, _) ->
  0.

%% If content coding doesn't have params 1
%% If params match 2
%% otherwise 0
score_params([], _) ->
  1;
score_params(CCParams, AltParams) when length(CCParams) == length(AltParams) ->
  case lists:sort(CCParams) == lists:sort(AltParams) of
    true -> 2;
    _ -> 0
  end;
score_params(_, _) ->
  0.
