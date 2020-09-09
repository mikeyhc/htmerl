-module(htmerl).

-include("htmerl.hrl").

-export([% render functions
         render/1, render_tag/1, render_tags/1,
         % document functions
         make_document/2,
         % general tags
         tag/2, tag/3,
         % attribute functions
         attribute/2, add_attribute/2, add_attributes/2,
         % standard tags
         body/1, div_tag/1, h1/1, head/1, link/0, span/1,
         % standard attributes
         class/1, href/1, id/1, rel/1, src/1, type/1
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
render_tag(T=#tag{name=N, content=[], can_empty=true}) ->
    Attrs = render_attributes(T#tag.attributes),
    <<"<", N/binary, " ", Attrs/binary, " \n/>">>;
render_tag(T=#tag{name=N, content=C}) ->
    Content = render_tags(C),
    Attrs = render_attributes(T#tag.attributes),
    <<"<", N/binary, " ", Attrs/binary, "\n>", Content/binary,
      "</", N/binary, "\n>">>.

-spec bin_join(binary(), binary()) -> binary().
bin_join(X, Y) -> <<Y/binary, X/binary>>.

-spec bin_join_space(binary(), binary()) -> binary().
bin_join_space(X, Y) -> <<X/binary, " ", Y/binary>>.

-spec render_tags([tag()]) -> binary().
render_tags(L) ->
    lists:foldl(fun bin_join/2, <<>>, lists:map(fun render_tag/1, L)).

-spec render_attributes([attribute()]) -> binary().
render_attributes(L) ->
    lists:foldl(fun bin_join_space/2, <<>>,
                lists:map(fun render_attribute/1, L)).

-spec render_attribute(attribute()) -> binary().
render_attribute(#attribute{name=N, value=V}) ->
    <<N/binary, "=\'", V/binary, "\'">>.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Document Functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec make_document(tag() | [tag()], html() | [html()]) -> document().
make_document(Head, Body) -> #document{head=Head, body=Body}.

%%%%%%%%%%%%%%%%%%%%%
%%% Tag Functions %%%
%%%%%%%%%%%%%%%%%%%%%

flip(F) -> fun(X, Y) -> F(Y, X) end.

-spec attribute(binary(), binary()) -> attribute().
attribute(Name, Value) -> #attribute{name=Name, value=Value}.

-spec add_attribute(tag(), attribute()) -> tag().
add_attribute(T, A) -> T#tag{attributes=[A|T#tag.attributes]}.

-spec add_attributes(tag(), [attribute()]) -> tag().
add_attributes(T, L) -> lists:foldl(flip(fun add_attribute/2), T, L).

%%%%%%%%%%%%%%%%%%%%
%%% General Tags %%%
%%%%%%%%%%%%%%%%%%%%

-spec tag(binary(), html() | [html()]) -> tag().
tag(Name, Value) -> tag(Name, Value, []).

-spec tag(binary(), html() | [html()], [tag_option()]) -> tag().
tag(Name, Value, Opts) when is_list(Value) ->
    PreTag = lists:foldl(fun handle_tag_opts/2, #tag{}, Opts),
    PreTag#tag{name=Name, content=Value};
tag(Name, no_html, Opts) -> tag(Name, [], Opts);
tag(Name, Value, Opts) -> tag(Name, [Value], Opts).

-spec handle_tag_opts(tag_option(), tag()) -> tag().
handle_tag_opts({can_empty, V}, Tag) -> Tag#tag{can_empty=V};
handle_tag_opts(T,  _) -> error({invalid_tag_options, T}).

%%%%%%%%%%%%%%%%%%%%%
%%% Standard Tags %%%
%%%%%%%%%%%%%%%%%%%%%

-spec body(html() | [html()]) -> tag().
body(Tags) -> tag(<<"body">>, Tags, [{can_empty, false}]).

-spec div_tag(html() | [html()]) -> tag().
div_tag(Tags) -> tag(<<"div">>, Tags, [{can_empty, false}]).

-spec h1(binary()) -> tag().
h1(Value) -> tag(<<"h1">>, Value).

-spec head(tag() | [tag()]) -> tag().
head(Tags) -> tag(<<"head">>, Tags, [{can_empty, false}]).

-spec link() -> tag().
link() -> tag(<<"link">>, no_html).

-spec span(html() | [html()]) -> tag().
span(Tag) -> tag(<<"span">>, Tag).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Standard Attributes %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec class(binary()) -> attribute().
class(V) -> attribute(<<"class">>, V).

-spec href(binary()) -> attribute().
href(V) -> attribute(<<"href">>, V).

-spec id(binary()) -> attribute().
id(V) -> attribute(<<"id">>, V).

-spec rel(binary()) -> attribute().
rel(V) -> attribute(<<"rel">>, V).

-spec src(binary()) -> attribute().
src(V) -> attribute(<<"src">>, V).

-spec type(binary()) -> attribute().
type(V) -> attribute(<<"type">>, V).
