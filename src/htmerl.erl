-module(htmerl).

-include("htmerl.hrl").

-export([% render functions
         render/1, render_tag/1, render_tags/1,
         % document functions
         make_document/2,
         % general tags
         tag/2, tag/3,
         % standard tags
         body/1, h1/1, head/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Render Functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%

-spec render(document()) -> binary().
render(#document{doctype=DT, head=H, body=B}) ->
    Head = render_tag(H),
    Body = render_tag(B),
    <<"<!DOCTYPE ", DT/binary, ">\n\n", Head/binary, Body/binary>>.

-spec render_tag(no_html | tag()) -> binary().
render_tag(no_html) -> <<>>;
render_tag(B) when is_binary(B) -> B;
render_tag(#tag{name=N, content=[], can_empty=true}) ->
    <<"<", N/binary, "\n/>">>;
render_tag(#tag{name=N, content=no_html, can_empty=true}) ->
    <<"<", N/binary, "\n/>">>;
render_tag(#tag{name=N, content=C}) ->
    Content = render_tags(C),
    <<"<", N/binary, "\n>", Content/binary, "</", N/binary, "\n>">>.

-spec render_tags([tag()]) -> binary().
render_tags(L) ->
    lists:foldl(fun(X, A) -> <<X/binary, A/binary>> end, <<>>,
                lists:map(fun render_tag/1, L)).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Document Functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec make_document(tag() | [tag()], html() | [html()]) -> document().
make_document(Head, Body) -> #document{head=Head, body=Body}.

%%%%%%%%%%%%%%%%%%%%
%%% General Tags %%%
%%%%%%%%%%%%%%%%%%%%

-spec tag(binary(), html() | [html()]) -> tag().
tag(Name, Value) -> tag(Name, Value, []).

-spec tag(binary(), html() | [html()], [tag_option()]) -> tag().
tag(Name, Value, Opts) when is_list(Value) ->
    PreTag = lists:foldl(fun handle_tag_opts/2, #tag{}, Opts),
    PreTag#tag{name=Name, content=Value};
tag(Name, Value, Opts) -> tag(Name, [Value], Opts).

-spec handle_tag_opts(tag_option(), tag()) -> tag().
handle_tag_opts({can_empty, V}, Tag) -> Tag#tag{can_empty=V};
handle_tag_opts(T,  _) -> error({invalid_tag_options, T}).

%%%%%%%%%%%%%%%%%%%%%
%%% Standard Tags %%%
%%%%%%%%%%%%%%%%%%%%%

-spec body(html() | [html()]) -> tag().
body(Tags) -> tag(<<"body">>, Tags, [{can_empty, false}]).

-spec h1(binary()) -> tag().
h1(Value) -> tag(<<"h1">>, Value).

-spec head(tag() | [tag()]) -> tag().
head(Tags) -> tag(<<"head">>, Tags, [{can_empty, false}]).
