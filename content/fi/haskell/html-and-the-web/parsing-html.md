---
title:                "HTML:n jäsennys"
aliases:
- /fi/haskell/parsing-html/
date:                  2024-02-03T19:12:24.243055-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML:n jäsennys"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mitä & Miksi?

HTML:n jäsentäminen Haskellissa mahdollistaa datan poiminnan, HTML-sisällön manipuloinnin tai ohjelmallisesti verkkosivujen kanssa vuorovaikuttamisen. Tämä toiminto on olennainen tehtävissä, kuten verkon raaputtamisessa (web scraping), verkkosovellusten automatisoidussa testaamisessa ja tietojen louhimisessa verkkosivustoilta - hyödyntäen Haskellin vahvaa tyyppijärjestelmää ja funktionaalisen ohjelmoinnin paradigmoja varmistaakseen kestävän ja ytimekään koodin.

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
