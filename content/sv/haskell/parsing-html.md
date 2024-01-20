---
title:                "Analysera html"
html_title:           "Arduino: Analysera html"
simple_title:         "Analysera html"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att analysera HTML innebär att granska och utvinna bestämd data från HTML-koden. Programmerare gör detta för att skrapa webbdata, automatisera innehåll eller för att upptäcka specifika element.

## Hur man gör

Vi kommer att använda `tagsoup` biblioteket för att analysera HTML. Låt oss börja med att importera det:

```Haskell
import Text.HTML.TagSoup
```

I följande exempel parsar vi en enkel HTML-sträng:

```Haskell
let html = "<html><body><p>Hello, world!</p></body></html>"
let tags = parseTags html
```
`parseTags` kommer att ge oss en lista med taggar, element och attribut:

```Haskell
[TagOpen "html" [], TagOpen "body" [], TagOpen "p" [], TagText "Hello, world!", TagClose "p", TagClose "body", TagClose "html"]
```

För att söka efter specifika taggar, kan vi använda `sections` funktionen. Låt oss hitta vår `p`-tagg:

```Haskell
let paragraphSections = sections (~== "<p>") tags
```

Detta ger oss alla element inom `p`-taggen:

```Haskell
[[TagOpen "p" [], TagText "Hello, world!", TagClose "p"]]
```

## Djupdykning

HTML-parsning har sitt ursprung i början av webbens tid, när det var nödvändigt att utvinna data från primitiva och ofta inkonsekventa HTML sidor. Det finns flera alternativ till `tagsoup`, till exempel `html-conduit` eller `tagsoup-parsec`.

När det gäller utförande behandlar `tagsoup` HTML som en sekvens av öppna och stängda taggar, snarare än ett uppbyggt dom-träd. Det betyder att det kan bearbeta inkonsekvent och bruten HTML snabbare och enklare än några andra bibliotek.

## Se också

1. [Hackage: TagSoup](http://hackage.haskell.org/package/tagsoup)
2. [School of Haskell: Parsing HTML](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/tagsoup)