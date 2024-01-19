---
title:                "Analysera html"
html_title:           "Arduino: Analysera html"
simple_title:         "Analysera html"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att parsa HTML handlar om att omvandla HTML-kod till en datastruktur som ett program kan förstå och bearbeta. Programmers gör detta för att de kan interagera med webbinnehåll på ett mer nyanserat sätt, till exempel extrahera specifik data, manipulera sidinnehåll eller implementera webbskrapning.

## Hur man gör:

Här är ett Elm-program som parar en HTML-sträng genom att använda `Html.Parser`-biblioteket. 

```Elm
import Html.Parser exposing (..)
import Html.Parser.Util exposing (tag)

parseHtml : String -> Result Parser.Error (List Parser.Step)
parseHtml =
    parse <| oneOf [ tag "h1", tag "p" ]

example : String
example =
    """
    <h1>Hello world!</h1>
    <p>Welcome to Elm.</p>
    """ 

main = 
    case parseHtml example of
        Ok result ->
            -- process the result
        Err _ ->
            -- handle the error
```

Kör du det programmet kommer du få en lista av `Parser.Step` som representar h1- och p-taggar i HTML-strängen.

## Fördjupning:

Historiskt sett har HTML-parsing utförts på serversidan med hjälp av språk som PHP och Java. Som en kompileringsspråk möjliggör Elm parsing av HTML på klientsidan, vilket kan vara mer effektivt beroende på användningsfallet.

Alternativen till att parsa HTML med Elm inkluderar att använda JavaScript-bibliotek som JSDOM eller Cheerio.

När vi pratar om implementeringsdetaljer använder Elm-bibliotek som `Html.Parser` en teknik som kallas för "recursive descent parsing". Det här tillvägagångssättet bygger parse trädet rekursivt nerifrån och upp genom att matcha HTML-strängen med en serie av försök till parsers definierade i programmet.

## Se även:

- [Elm's Html.Parser modul](https://package.elm-lang.org/packages/elm/html/latest/Html-Parser): Det huvudsakliga biblioteket för att parsa HTML i Elm.
- [Elm's Guide to Parsing](https://guide.elm-lang.org/parsing.html): En grundlig genomgång av parsing i Elm, inklusive HTML-parsing.
- [Parser Combinators in Elm](https://medium.com/@_rchaves_/parser-combinators-in-elm-22654ffd02f2): En djupare dykning i parser combinators, som är grunden för Elm's parsing bibliotek.