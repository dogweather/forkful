---
title:                "Analysera HTML"
html_title:           "Elm: Analysera HTML"
simple_title:         "Analysera HTML"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/parsing-html.md"
---

{{< edit_this_page >}}

## Vad och varför?
HTML är ett vanligt format för att strukturera och presentera innehåll på webben. När webbprogrammerare behöver arbeta med HTML-koden, är det ofta nödvändigt att "parsa" (tolka) den för att kunna förstå och manipulera den. Parsing HTML är alltså en metod för att extrahera och organisera information från en HTML-sida.

## Hur man gör:
Att parsa HTML i Elm är relativt enkelt, tack vare det inbyggda paketet elm/parser. Nedan finns ett exempel på hur man tolkar en HTML-sida och extraherar all text från alla p-taggar:

```Elm
import Html.Attributes exposing (attribute)
import Html.Parser exposing (root, map, maybe, oneOf, text, tag, many)

type alias HtmlPage =
  { title : String
  , body : String
  }

htmlParser : Parser m HtmlPage
htmlParser =
  map2 (\title body -> { title = title, body = body })
    (text (tag "title"))
    (many (text (tag "p")))

getPage : String -> Maybe HtmlPage
getPage html =
  case htmlParser |> root (Maybe.map .words) html of
    Ok page ->
      Just page
    Err _ ->
      Nothing
```

## Djupdykning:
Parsing är en viktig del av webbutveckling, eftersom det gör det möjligt att hämta information från en hemsida och använda den på andra sätt. Det finns dock alternative metoder för detta, som till exempel web scraping, vilket innebär att man hämtar data från webbsidor genom att simulera en webbläsare. Detta är dock oftast mer komplicerat och kräver mer hantering av JavaScript-kod.

Elm/javascript-parsers används ofta för att tillhandahålla en mer strukturerad representation av HTML-koden, och är därför användbart vid till exempel web scraping och automatiserade testning.

## Se även:
- [Elm/parser dokumentation](https://package.elm-lang.org/packages/elm/parser/latest/)
- [HTML to Elm konverterare](https://mbylstra.github.io/html-to-elm/)
- [Web scraping med Python](https://realpython.com/beautiful-soup-web-scraper-python/)
- [Åtkomst av HTML-dokument i olika webbläsare](https://www.w3.org/2007/10/htmldocument.html)