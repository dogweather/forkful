---
title:                "Analysering av html"
html_title:           "C#: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/parsing-html.md"
---

{{< edit_this_page >}}

# Parsing HTML i Haskell

## Hva & Hvorfor?

Parsing av HTML handler om å analysere HTML-kode for å forstå dens struktur og innhold. Programmerere gjør det for å høste, manipulere, eller presentere data hentet fra HTML-dokumenter.

## Hvordan:

Først, installer `tagsoup` biblioteket ved å kjøre dette i terminalen:

```Haskell
cabal install tagsoup
```

La oss lage en enkel parser som henter alle lenkene fra en HTML-tekst:

```Haskell
import Text.HTML.TagSoup

lenkeParser :: String -> [String]
lenkeParser html = [href | TagOpen "a" atts <- parseTags html, ("href",href) <- atts]
```

Bruk det slik:

```Haskell
print $ lenkeParser "<a href='https://www.example.com'>Eksempel</a>"
```
Dette vil gi resultatet:

```Haskell
["https://www.example.com"]
```

## Dyp Dykk

**Historisk Kontekst**: Parsing av HTML har vært sentralt siden World Wide Webs fødsel, å kunne ha innsikt i hvordan nettsider innhold og struktur er framstilt har en rekke applikasjoner. Først og fremst innen data mining, web scraping, og web-crawling.

**Alternativer**: Det er mange alternative biblioteker for HTML-parsing i Haskell, som `html-conduit`, og `hxt`. Hvert med sine egne fordeler og ulemper.

**Implementeringsdetaljer**: Under panseret konverterer `TagSoup`-biblioteket HTML-strengen til en strøm av merkelapper. Disse merkelappene kan være åpne koder (f.eks. `<p>`), lukkede koder (f.eks. `</p>`), eller tekst. Deretter brukes listekomprimering for å filtrere ut og hente verdier fra disse taggene.

## Se Også

- [TagSoup på Hackage](https://hackage.haskell.org/package/tagsoup)
- [Andre parsing biblioteker på Hackage](https://hackage.haskell.org/packages/#cat:Parsing)