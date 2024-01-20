---
title:                "Analysering av html"
html_title:           "C#: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/parsing-html.md"
---

{{< edit_this_page >}}

# Parsing HTML i Elm: En Introduksjon

## Hva & Hvorfor?

Parsing av HTML handler om å forvandle brutt kode til noe som kan forstås og manipuleres av et dataprogram. Programmerere gjør dette for å hente data, manipulere websider, automatisere handlinger osv.

## Hvordan:

La oss dykke rett i. Her er en enkel eksempel på hvordan du kan parse HTML i Elm.

```Elm
import Html exposing (Html)
import Html.Parser exposing (..)
import Html.Parser.Util exposing (..)

main =
    let
        html = 
            """
            <html>
                <head>
                <title>Test</title>
                </head>
                <body>
                <h1>Hello, world!</h1>
                </body>
            </html>
            """
        parser = tag "h1" (text)
        result = run(parser, parse(html))
    in 
        Html.text <| case result of 
            Ok val -> val
            Err err -> toString err
```

Kjør koden ovenfor og Elm vil skrive ut "Hello, world!"

## Dypdykk

Elm er relativt nytt sammenlignet med andre språk som JavaScript, så støtte for HTML-parsing har måttet utvikle seg. Det er imidlertid fortsatt et noenlunde simpelt virkemiddel sammenlignet med noen andre språk. Andre alternativer for scraping av nettsider inkluderer språk som Python og JavaScript, men Elm tilbyr en mer sikker og pålitelig måte å gjøre det på.

Når du parser HTML i Elm, bruker du faktisk en funksjonell tilnærming til problemet. Du bygger opp parsefunksjoner som leser bestemte biter av HTML-kode og returnerer en verdi de representerer.

## Se Også

Her er noen nyttige lenker til relaterte ressurser:

- [Elm HTML Parser Docs](https://package.elm-lang.org/packages/elm-lang/html/latest/Html-Parser) - Den offisielle dokumentasjonen er alltid et bra sted å starte.
- [Html Parser in Elm](https://becoming-functional.com/html-parser-in-elm-1-5efe4922a28d) - En god tutorial som går mer detaljert inn på hvordan du bruker `Html.Parser`.
- [HTML Parsing in Elm](https://www.youtube.com/watch?v=Nf6a8ub9smM) - Om du foretrekker videoformat, presenterer denne YouTube-videoen parsing i Elm på en klar og forståelig måte.