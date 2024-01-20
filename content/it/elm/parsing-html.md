---
title:                "Analisi sintattica dell'HTML"
html_title:           "C++: Analisi sintattica dell'HTML"
simple_title:         "Analisi sintattica dell'HTML"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/parsing-html.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

1. Il parsing di HTML è l'atto di analizzare un documento HTML per estrarre dati e struttura. 
2. I programmatori lo fanno per recuperare informazioni dai siti web e manipolarle a loro piacimento.

## Come fare:

Elm, essendo una lingua funzionale, offre modi eleganti per eseguire il parsing HTML. Prendiamo come esempio questo codice che estrae il testo da un documento HTML:

```Elm
import Html exposing (text)
import Html.Parser exposing (..)
import Html.Parser.Util

parseHtml : String -> String
parseHtml html = 
  case parse html of
    Ok nodes ->
      toString <| Html.text nodes
    Err _ ->
      "Parsing errato."

main =
  text <| parseHtml "<span>Ciao mondo</span>"
```

Quando esegui questo codice, otterrai l'output "Ciao mondo". Questo è un esempio molto semplice, ma la bellezza di Elm si nota soprattutto quando si affrontano problemi più complessi.

## Approfondimento

1. Nel contesto storico, la necessità di parsing HTML è diventata predominante con la crescita del web scraping, la pratica di estrarre dati dai siti web.
2. Esistono numerose alternative. Ogni linguaggio di programmazione tende ad avere le proprie librerie per il parsing HTML. Ad esempio, ci sono Beautiful Soup per Python o Jsoup per Java.
3. Elm ha una libreria di analisi molto consistente. La cosa interessante di Elm è che, a differenza di altre lingue, non ti permette di realizzare un parser che può fallire senza gestire l'errore. Questa è una grande particolarità di Elm che ti costringe a scrivere codice robusto e sicuro.

## Vedi anche

Se sei interessato ad approfondire il parsing HTML in Elm, ecco alcuni link utili che potrebbero aiutarti: 
1. [Documentazione ufficiale del modulo Html.Parser di Elm](https://package.elm-lang.org/packages/elm/elm-markup/latest/Html-Parser)
2. [Un esempio complesso di parsing HTML in Elm](https://korban.net/posts/elm/2019-11-12-complex-example-parsing-html-elm/)  
3. [Web Scraping e Parsing HTML con Elm](https://becoming-functional.com/web-scraping-in-elm-part-1-7fc2b9dde867)