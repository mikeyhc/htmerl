-module(htmerl_tests).

-include_lib("eunit/include/eunit.hrl").
-include("htmerl.hrl").

-compile([export_all]).


render_test_() ->
    [ ?_assertEqual(<<"<!DOCTYPE html>\n\n"
                      "<html \n>"
                      "<head \n>"
                        "<link href='default'  \n/>"
                      "</head\n>"
                      "<body \n>"
                      "body"
                      "</body\n>"
                      "</html\n>">>,
                      htmerl:render(
                        htmerl:make_document(
                          htmerl:head(
                            htmerl:add_attributes(htmerl:link(),
                                                  [htmerl:href(<<"default">>)
                                                  ])),
                          htmerl:body(<<"body">>)))),
      ?_assertEqual(<<>>, htmerl:render_tag(no_html))
    ].


document_test_() ->
    [ ?_assertEqual(#document{head=#tag{name= <<"head">>, can_empty=false},
                              body=#tag{name= <<"body">>, can_empty=false}},
                    htmerl:make_document(htmerl:head(no_html),
                                         htmerl:body(no_html)))
    ].

add_attribute_test_() ->
    [ ?_assertEqual(#tag{name= <<"link">>,
                         attributes=
                            [ #attribute{name= <<"href">>,
                                         value=default}
                            ]},
                    htmerl:add_attribute(htmerl:link(),
                                         htmerl:href(default))),
      ?_assertEqual(#tag{name= <<"link">>,
                         attributes=
                            [ #attribute{name= <<"href">>,
                                         value=default}
                            ]},
                    htmerl:add_attributes(htmerl:link(),
                                          [htmerl:href(default)]))
    ].

%%%%%%%%%%%%%%%%%
%%% Tag Tests %%%
%%%%%%%%%%%%%%%%%

no_html_tag(Title, F) ->
    ?_assertEqual(#tag{name= Title}, F(no_html)).

no_html_ne_tag(Title, F) ->
    ?_assertEqual(#tag{name= Title, can_empty=false}, F(no_html)).

tags_test_() ->
    [ no_html_tag(<<"test">>, fun(X) -> htmerl:tag(<<"test">>, X) end),
      ?_assertEqual(#tag{name= <<"test">>, content= [a]},
                    htmerl:tag(<<"test">>, a)),
      ?_assertError({invalid_tag_options, x},
                    htmerl:tag(<<"test">>, no_html, [x])),
      lists:map(fun({X, Y}) -> no_html_ne_tag(Y, fun htmerl:X/1) end,
                [ {anchor, <<"a">>},
                  {body, <<"body">>},
                  {div_tag, <<"div">>},
                  {head, <<"head">>},
                  {html, <<"html">>}
                ]),
      no_html_tag(<<"link">>, fun(_) -> htmerl:link() end),
      lists:map(fun({X, Y}) -> no_html_tag(Y, fun htmerl:X/1) end,
                [ {h1, <<"h1">>},
                  {span, <<"span">>},
                  {table, <<"table">>},
                  {tbody, <<"tbody">>},
                  {td, <<"td">>},
                  {title, <<"title">>},
                  {tr, <<"tr">>}
                ])
    ].


%%%%%%%%%%%%%%%%%%%%%%%
%%% Attribute Tests %%%
%%%%%%%%%%%%%%%%%%%%%%%

default_attribute(Name, F) ->
    ?_assertEqual(#attribute{name=Name, value=default}, F(default)).

attribute_test_() ->
    [ default_attribute(<<"test">>,
                        fun(X) -> htmerl:attribute(<<"test">>, X) end),
      lists:map(fun({X, Y}) -> default_attribute(Y, fun htmerl:X/1) end,
                [{class, <<"class">>},
                 {href, <<"href">>},
                 {id, <<"id">>},
                 {rel, <<"rel">>},
                 {src, <<"src">>},
                 {type, <<"type">>}
                ])
    ].
