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
         anchor/1, body/1, div_tag/1, form/1, h1/1, head/1, html/1,
         input/1, link/0, option/1, select/1, span/1, submit/1,
         table/1, tbody/1, td/1, textarea/1, title/1, tr/1,
         article/1, aside/1, details/1, figcaption/1, figure/1, footer/1,
         header/1, main/1, mark/1, nav/1, section/1, summary/1, time/1,
         % standard attributes
         action/1, class/1, href/1, id/1, method/1, multiple/0, name/1,
         onclick/1, rel/1, size/1, src/1, type/1, value/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Render Functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%

-spec render(document()) -> binary().
render(#document{doctype=DT, head=H, body=B}) ->
    HTML = render_tag(htmerl:html([H, B])),
    <<"<!DOCTYPE ", DT/binary, ">\n\n", HTML/binary>>.

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
    AgList = aggregate_attrs(L),
    lists:foldl(fun bin_join_space/2, <<>>,
                lists:map(fun render_attribute/1, AgList)).

-spec render_attribute(attribute()) -> binary().
render_attribute(#attribute{name=N, value=empty}) -> N;
render_attribute(#attribute{name=N, value=V}) ->
    <<N/binary, "=\'", V/binary, "\'">>.

-spec aggregate_attrs([attribute()]) -> [attribute()].
aggregate_attrs(L) ->
    lists:foldl(fun aggregate_attr/2, [], L).

-spec aggregate_attr(attribute(), [attribute()]) -> [attribute()].
aggregate_attr(Attr, List) ->
    Match = fun(X) -> same_attr(Attr, X) end,
    case remove(Match, List) of
        {E, L} -> [join_attr(E, Attr)|L];
        List   -> [Attr|List]
    end.

-spec remove(fun((T) -> boolean()), [T]) -> {T, [T]} | [T].
remove(_Fun, []) -> [];
remove(Fun, [H|T]) ->
    case Fun(H) of
        true -> {H, T};
        _    ->
           case remove(Fun, T) of
               {R, NT} -> {R, [H|NT]};
               List    -> [H|List]
           end
    end.

-spec same_attr(attribute(), attribute()) -> boolean().
same_attr(A1, A2) -> A1#attribute.name =:= A2#attribute.name.

-spec join_attr(attribute(), attribute()) -> attribute().
join_attr(A1, A2) ->
    A1V = A1#attribute.value,
    A2V = A2#attribute.value,
    A1#attribute{value=bin_join_space(A2V, A1V)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Document Functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec make_document(tag() | [tag()], html() | [html()]) -> document().
make_document(Head, Body) -> #document{head=Head, body=Body}.

%%%%%%%%%%%%%%%%%%%%%
%%% Tag Functions %%%
%%%%%%%%%%%%%%%%%%%%%

flip(F) -> fun(X, Y) -> F(Y, X) end.

-spec attribute(binary(), binary() | empty) -> attribute().
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

-spec anchor(html() | [html()]) -> tag().
anchor(Tags) -> tag(<<"a">>, Tags, [{can_empty, false}]).

-spec body(html() | [html()]) -> tag().
body(Tags) -> tag(<<"body">>, Tags, [{can_empty, false}]).

-spec div_tag(html() | [html()]) -> tag().
div_tag(Tags) -> tag(<<"div">>, Tags, [{can_empty, false}]).

-spec form(html() | [html()]) -> tag().
form(Tags) -> tag(<<"form">>, Tags, [{can_empty, false}]).

-spec h1(binary()) -> tag().
h1(Value) -> tag(<<"h1">>, Value).

-spec head(tag() | [tag()]) -> tag().
head(Tags) -> tag(<<"head">>, Tags, [{can_empty, false}]).

-spec html(tag() | [tag()]) -> tag().
html(Tags) -> tag(<<"html">>, Tags, [{can_empty, false}]).

-spec input(html() | [html()]) -> tag().
input(Tags) -> tag(<<"input">>, Tags).

-spec link() -> tag().
link() -> tag(<<"link">>, no_html).

-spec option(binary()) -> tag().
option(Value) -> tag(<<"option">>, Value, [{can_empty, false}]).

-spec select(tag() | [tag()]) -> tag().
select(Tags) -> tag(<<"select">>, Tags, [{can_empty, false}]).

-spec span(html() | [html()]) -> tag().
span(Tag) -> tag(<<"span">>, Tag).

-spec submit(html() | [html()]) -> tag().
submit(Tag) -> tag(<<"submit">>, Tag, [{can_empty, false}]).

-spec table(tag() | [tag()]) -> tag().
table(Tag) -> tag(<<"table">>, Tag).

-spec tbody(tag() | [tag()]) -> tag().
tbody(Tag) -> tag(<<"tbody">>, Tag).

-spec td(html() | [html()]) -> tag().
td(Value) -> tag(<<"td">>, Value, [{can_empty, false}]).

-spec textarea(binary()) -> tag().
textarea(Value) -> tag(<<"textarea">>, Value, [{can_empty, false}]).

-spec title(binary()) -> tag().
title(Value) -> tag(<<"title">>, Value).

-spec tr(tag() | [tag()]) -> tag().
tr(Tag) -> tag(<<"tr">>, Tag, [{can_empty, false}]).

-spec article(tag() | [tag()]) -> tag().
article(Tag) -> tag(<<"article">>, Tag, [{can_empty, false}]).

-spec aside(tag() | [tag()]) -> tag().
aside(Tag) -> tag(<<"aside">>, Tag, [{can_empty, false}]).

-spec details(tag() | [tag()]) -> tag().
details(Tag) -> tag(<<"details">>, Tag, [{can_empty, false}]).

-spec figcaption(tag() | [tag()]) -> tag().
figcaption(Tag) -> tag(<<"figcaption">>, Tag, [{can_empty, false}]).

-spec figure(tag() | [tag()]) -> tag().
figure(Tag) -> tag(<<"figure">>, Tag, [{can_empty, false}]).

-spec footer(tag() | [tag()]) -> tag().
footer(Tag) -> tag(<<"footer">>, Tag, [{can_empty, false}]).

-spec header(tag() | [tag()]) -> tag().
header(Tag) -> tag(<<"header">>, Tag, [{can_empty, false}]).

-spec main(tag() | [tag()]) -> tag().
main(Tag) -> tag(<<"main">>, Tag, [{can_empty, false}]).

-spec mark(tag() | [tag()]) -> tag().
mark(Tag) -> tag(<<"mark">>, Tag, [{can_empty, false}]).

-spec nav(tag() | [tag()]) -> tag().
nav(Tag) -> tag(<<"nav">>, Tag, [{can_empty, false}]).

-spec section(tag() | [tag()]) -> tag().
section(Tag) -> tag(<<"section">>, Tag, [{can_empty, false}]).

-spec summary(tag() | [tag()]) -> tag().
summary(Tag) -> tag(<<"summary">>, Tag, [{can_empty, false}]).

-spec time(tag() | [tag()]) -> tag().
time(Tag) -> tag(<<"time">>, Tag, [{can_empty, false}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Standard Attributes %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec action(binary()) -> attribute().
action(V) -> attribute(<<"action">>, V).

-spec class(binary()) -> attribute().
class(V) -> attribute(<<"class">>, V).

-spec href(binary()) -> attribute().
href(V) -> attribute(<<"href">>, V).

-spec id(binary()) -> attribute().
id(V) -> attribute(<<"id">>, V).

-spec method(binary()) -> attribute().
method(V) -> attribute(<<"method">>, V).

-spec multiple() -> attribute().
multiple() -> attribute(<<"multiple">>, empty).

-spec name(binary()) -> attribute().
name(V) -> attribute(<<"name">>, V).

-spec onclick(binary()) -> attribute().
onclick(V) -> attribute(<<"onclick">>, V).

-spec rel(binary()) -> attribute().
rel(V) -> attribute(<<"rel">>, V).

-spec size(binary()) -> attribute().
size(V) -> attribute(<<"size">>, V).

-spec src(binary()) -> attribute().
src(V) -> attribute(<<"src">>, V).

-spec type(binary()) -> attribute().
type(V) -> attribute(<<"type">>, V).

-spec value(binary()) -> attribute().
value(V) -> attribute(<<"value">>, V).
