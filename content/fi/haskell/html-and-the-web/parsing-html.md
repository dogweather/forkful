---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:24.243055-07:00
description: "Miten: HTML:n j\xE4sent\xE4miseen Haskellissa k\xE4yt\xE4mme `tagsoup`-kirjastoa\
  \ sen yksinkertaisuuden ja joustavuuden vuoksi. Varmista ensin, ett\xE4 asennat\
  \ kirjaston\u2026"
lastmod: '2024-03-13T22:44:56.613479-06:00'
model: gpt-4-0125-preview
summary: "HTML:n j\xE4sent\xE4miseen Haskellissa k\xE4yt\xE4mme `tagsoup`-kirjastoa\
  \ sen yksinkertaisuuden ja joustavuuden vuoksi."
title: "HTML:n j\xE4sennys"
weight: 43
---

## Miten:
HTML:n jäsentämiseen Haskellissa käytämme `tagsoup`-kirjastoa sen yksinkertaisuuden ja joustavuuden vuoksi. Varmista ensin, että asennat kirjaston lisäämällä `tagsoup` projektisi cabal-tiedostoon tai suorittamalla `cabal install tagsoup`.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.TagSoup

-- Esimerkki HTML demonstraatiota varten
let sampleHtml = "<html><body><p>Opiskele Haskellia!</p><a href='http://example.com'>Klikkaa tästä</a></body></html>"

-- Jäsennä HTML ja suodata linkit (a-tunnisteet)
let tags = parseTags sampleHtml
let links = [fromAttrib "href" tag | tag <- tags, isTagOpenName "a" tag]

-- Tulosta poimitut linkit
print links
```

Esimerkkitulostus:
```plaintext
["http://example.com"]
```

Monimutkaisempiin HTML:n jäsentämisen tarpeisiin harkitse `pandoc`-kirjaston käyttämistä, erityisesti jos työskentelet dokumenttien muuntamisen parissa. Se on poikkeuksellisen monipuolinen, mutta tulee suuremmalla monimutkaisuudella:

```haskell
import Text.Pandoc

-- Olettaen, että sinulla on Pandoc-dokumentti (doc) ladattuna, esim. tiedoston lukemisen kautta
let doc = ... -- Pandoc-dokumenttisi tulee tähän

-- Muunna dokumentti HTML-merkkijonoksi
let htmlString = writeHtmlString def doc

-- Nyt sinun tulisi jäsentää `htmlString` kuten yllä tai jatkaa tarpeidesi mukaisesti.
```
Pidä mielessä, että `pandoc` on paljon suurempi kirjasto, joka keskittyy muuntamaan useiden merkkausmuotojen välillä, joten käytä sitä, jos tarvitset näitä ylimääräisiä ominaisuuksia tai jos työskentelet jo dokumenttiformaattien parissa sovelluksessasi.
