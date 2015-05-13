-record(document, {doctype= <<"html">> :: binary(),
                   head=no_html        :: no_html | tag(),
                   body=no_html        :: no_html | tag()}).
-type document() :: #document{}.

-record(tag, {name           :: binary(),
              attributes=[]  :: [attribute()],
              can_empty=true :: boolean(),
              content=[]     :: [tag() | binary()]}).
-type tag() :: #tag{}.

-record(attribute, {name  :: binary(),
                    value :: binary()}).
-type attribute() :: #attribute{}.

-type html() :: tag() | binary() | no_html.

-type tag_option() :: {can_empty, boolean()}.
