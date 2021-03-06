-module(htmerl_tests).

-include_lib("eunit/include/eunit.hrl").
-include("htmerl.hrl").

render_test_() ->
    [ ?_assertEqual(["<!DOCTYPE ",<<"html">>,">\n\n",
                     ["<",<<"html">>," ",[],"\n>",
                      [["<",<<"head">>," ",[],"\n>",
                        [["<",<<"link">>," ",
                          [[<<"href">>,"='",<<"default">>,"'"]],
                          " \n/>"]],
                        "</",<<"head">>,"\n>"],
                       ["<",<<"body">>," ",[],"\n>",
                        [<<"body">>],
                        "</",<<"body">>,"\n>"]],
                      "</",<<"html">>,"\n>"]],
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
                                          [htmerl:href(default)])),
      ?_assertEqual(["<",<<"link">>," ",[<<"multiple">>]," \n/>"],
                    htmerl:render_tag(
                      htmerl:add_attribute(htmerl:link(),
                                           htmerl:multiple())))
    ].

join_attribute_test_() ->
    [ ?_assertEqual(["<",<<"link">>," ",
                     [[<<"class">>,"='",[<<"a">>," ",<<"b">>],"'"]],
                     " \n/>"],
                    htmerl:render_tag(
                      htmerl:add_attributes(htmerl:link(),
                                            [ htmerl:class(<<"a">>),
                                              htmerl:class(<<"b">>)
                                            ]))),
      ?_assertEqual(["<",<<"link">>," ",
                     [[<<"class">>,"='",[<<"b">>," ",<<"c">>],"'"],
                      [<<"href">>,"='",<<"a">>,"'"]],
                     " \n/>"],
                    htmerl:render_tag(
                      htmerl:add_attributes(htmerl:link(),
                                            [ htmerl:class(<<"b">>),
                                              htmerl:href(<<"a">>),
                                              htmerl:class(<<"c">>)
                                            ])))
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
      lists:map(fun({X, Y}) -> no_html_ne_tag(Y, X) end,
                [ {fun htmerl:anchor/1, <<"a">>},
                  {fun htmerl:body/1, <<"body">>},
                  {fun htmerl:div_tag/1, <<"div">>},
                  {fun htmerl:form/1, <<"form">>},
                  {fun htmerl:head/1, <<"head">>},
                  {fun htmerl:html/1, <<"html">>},
                  {fun htmerl:option/1, <<"option">>},
                  {fun htmerl:select/1, <<"select">>},
                  {fun htmerl:submit/1, <<"submit">>},
                  {fun htmerl:td/1, <<"td">>},
                  {fun htmerl:textarea/1, <<"textarea">>},
                  {fun htmerl:tr/1, <<"tr">>},

                  {fun htmerl:article/1, <<"article">>},
                  {fun htmerl:aside/1, <<"aside">>},
                  {fun htmerl:details/1, <<"details">>},
                  {fun htmerl:figcaption/1, <<"figcaption">>},
                  {fun htmerl:figure/1, <<"figure">>},
                  {fun htmerl:footer/1, <<"footer">>},
                  {fun htmerl:header/1, <<"header">>},
                  {fun htmerl:main/1, <<"main">>},
                  {fun htmerl:mark/1, <<"mark">>},
                  {fun htmerl:nav/1, <<"nav">>},
                  {fun htmerl:section/1, <<"section">>},
                  {fun htmerl:summary/1, <<"summary">>},
                  {fun htmerl:time/1, <<"time">>}
                ]),
      no_html_tag(<<"link">>, fun(_) -> htmerl:link() end),
      lists:map(fun({X, Y}) -> no_html_tag(Y, X) end,
                [ {fun htmerl:h1/1, <<"h1">>},
                  {fun htmerl:h2/1, <<"h2">>},
                  {fun htmerl:h3/1, <<"h3">>},
                  {fun htmerl:h4/1, <<"h4">>},
                  {fun htmerl:h5/1, <<"h5">>},
                  {fun htmerl:h6/1, <<"h6">>},
                  {fun htmerl:input/1, <<"input">>},
                  {fun htmerl:span/1, <<"span">>},
                  {fun htmerl:table/1, <<"table">>},
                  {fun htmerl:tbody/1, <<"tbody">>},
                  {fun htmerl:title/1, <<"title">>}
                ])
    ].


%%%%%%%%%%%%%%%%%%%%%%%
%%% Attribute Tests %%%
%%%%%%%%%%%%%%%%%%%%%%%

default_attribute(Name, F) ->
    ?_assertEqual(#attribute{name=Name, value=default}, F(default)).

default_empty_attribute(Name, F) ->
    ?_assertEqual(#attribute{name=Name, value=empty}, F()).

attribute_test_() ->
    [ default_attribute(<<"test">>,
                        fun(X) -> htmerl:attribute(<<"test">>, X) end),
      lists:map(fun({X, Y}) -> default_attribute(Y, X) end,
                [{fun htmerl:action/1, <<"action">>},
                 {fun htmerl:class/1, <<"class">>},
                 {fun htmerl:href/1, <<"href">>},
                 {fun htmerl:id/1, <<"id">>},
                 {fun htmerl:method/1, <<"method">>},
                 {fun htmerl:name/1, <<"name">>},
                 {fun htmerl:onclick/1, <<"onclick">>},
                 {fun htmerl:rel/1, <<"rel">>},
                 {fun htmerl:size/1, <<"size">>},
                 {fun htmerl:src/1, <<"src">>},
                 {fun htmerl:type/1, <<"type">>},
                 {fun htmerl:value/1, <<"value">>}
                ]),
      lists:map(fun({X, Y}) -> default_empty_attribute(Y, X) end,
                [{fun htmerl:multiple/0, <<"multiple">>}
                ])
    ].
