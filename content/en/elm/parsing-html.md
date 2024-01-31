---
title:                "Parsing HTML"
date:                  2024-01-20T15:31:27.703931-07:00
html_title:           "Bash recipe: Parsing HTML"
simple_title:         "Parsing HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing HTML means converting HTML text into a data structure your program can work with. Programmers do this to manipulate, extract, and interact with the content of web pages programmatically.

## How to:
Elm doesn't parse raw HTML on its own out of the box; instead, it focuses on rendering views with its own `Html` module. To parse HTML, you'll typically use a server-side service or an external JavaScript library, then pass the data to Elm. Here's a basic Elm setup for dealing with parsed HTML data:

```Elm
module Main exposing (main)

import Html exposing (Html, text)
import Json.Decode as Decode

type alias HtmlData =
    { tag : String
    , attributes : List (String, String)
    , content : String
    }

-- Assuming you have a JSON object that represents your HTML data
htmlDataDecoder : Decode.Decoder HtmlData
htmlDataDecoder =
    Decode.map3 HtmlData
        (Decode.field "tag" Decode.string)
        (Decode.field "attributes" (Decode.list (Decode.tuple Decode.string Decode.string)))
        (Decode.field "content" Decode.string)

-- Replace with the actual JSON from a server or external JS library
sampleJson : String
sampleJson = 
    """
    {"tag":"div", "attributes": [["class", "container"]], "content": "Hello, Elm!"}
    """

-- Decoding the sample JSON to HtmlData
decodedHtmlData : Result Decode.Error HtmlData
decodedHtmlData =
    Decode.decodeString htmlDataDecoder sampleJson

-- Rendering a view from our HtmlData
view : HtmlData -> Html msg
view htmlData =
    Html.text (htmlData.content)

main : Html msg
main =
    case decodedHtmlData of
        Ok data ->
            view data
        
        Err error ->
            text "Failed to parse HTML data"

```

This dummy setup shows you how you'd start to work with parsed HTML data within Elm.

## Deep Dive
Historically, Elm's strong emphasis on type safety and a solid architecture means direct HTML parsing isn't its strong suit. Elm shines in building reliable web apps with minimal runtime errors.

For HTML parsing, you'd usually lean on JavaScript, which has mature libraries like `DOMParser` and jQuery's `$.parseHTML`. You'd do the heavy lifting in JavaScript, then ship the parse tree to Elm as Serialized JSON. You can turn to ports (Elm's way of communicating with JavaScript) or web services for that.

Once in Elm, JSON decoders turn serialized data into structured Elm types. With the JSON decode approach, you enjoy Elm's type safety and can keep messy HTML parsing logic outside your Elm codebase.

Alternatives? If you absolutely must parse HTML within Elm, you'll likely need a custom solution. It might involve using a server-side parser that exposes an API or an Elm package—if you find one that fits your needs, though choices are currently limited.

## See Also
For more on JSON decoding in Elm:
- The official Elm guide on JSON: https://guide.elm-lang.org/effects/json.html

Elm ports for JavaScript interop:
- Elm's guide on ports: https://guide.elm-lang.org/interop/ports.html

Community discussions and insights:
- Elm Discourse: https://discourse.elm-lang.org/
- Elm Slack channel, where you can ask for help and discuss: https://elmlang.herokuapp.com/
