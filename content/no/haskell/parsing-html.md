---
title:                "Analysering av HTML"
date:                  2024-02-03T19:12:15.608836-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analysering av HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å parse HTML i Haskell gir deg muligheten til å trekke ut data, manipulere HTML-innhold eller interagere med nettsider programmatisk. Denne operasjonen er essensiell for oppgaver som web scraping, automatisert testing av webapplikasjoner og datautvinning fra nettsteder - ved å utnytte Haskells sterke typesystem og paradigmer for funksjonell programmering for å sikre robust og kortfattet kode.

## Hvordan:

For å parse HTML i Haskell, vil vi bruke `tagsoup`-biblioteket for dets enkelhet og fleksibilitet. Først, sørg for å installere biblioteket ved å legge til `tagsoup` i ditt prosjekts cabal-fil eller ved å kjøre `cabal install tagsoup`.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.TagSoup

-- Eksempel HTML for demonstrasjon
let sampleHtml = "<html><body><p>Lær Haskell!</p><a href='http://example.com'>Klikk her</a></body></html>"

-- Parse HTML og filtrer for linker (a tags)
let tags = parseTags sampleHtml
let links = [fromAttrib "href" tag | tag <- tags, isTagOpenName "a" tag]

-- Skriv ut ekstraherte linker
print links
```

Eksempel på utskrift:
```plaintext
["http://example.com"]
```

For mer sofistikerte HTML-parsingbehov, vurder å bruke `pandoc`-biblioteket, spesielt hvis du jobber med dokumentkonvertering. Det er eksepsjonelt allsidig, men kommer med mer kompleksitet:

```haskell
import Text.Pandoc

-- Anta at du har et Pandoc-dokument (doc) lastet, f.eks. fra å lese en fil
let doc = ... -- Ditt Pandoc-dokument går her

-- Konverter dokumentet til HTML-streng
let htmlString = writeHtmlString def doc

-- Nå ville du parse `htmlString` som over eller fortsette som per dine krav.
```
Husk at `pandoc` er et mye større bibliotek som fokuserer på konvertering mellom tallrike markup-formater, så bruk det hvis du trenger de ekstra evnene eller hvis du allerede jobber med dokumentformater i din applikasjon.
