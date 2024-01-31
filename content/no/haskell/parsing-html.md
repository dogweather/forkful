---
title:                "Analyse av HTML"
date:                  2024-01-20T15:32:21.911835-07:00
simple_title:         "Analyse av HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Parsing HTML betyr å tolke og forstå HTML-koden som en struktur å jobbe med i programmering. Programmerere gjør dette for å kunne hente ut data, manipulere innhold, eller integrere websider med software.

## Slik gjør du:
`hxt`-biblioteket i Haskell lar deg gjøre dette smertefritt. Her er et raskt eksempel:

```Haskell
import Text.XML.HXT.Core

parseHTML :: String -> IOStateArrow s b XmlTree
parseHTML html = readString [withParseHTML yes, withWarnings no] html

main :: IO ()
main = do
    htmlData <- readFile "eksempel.html"
    result <- runX $ parseHTML htmlData >>> deep (isElem >>> hasName "a") >>> getAttrValue "href"
    print result
```

Kjører du dette med en `eksempel.html` som inneholder linker, vil output bli en liste av `href`-verdier fra `<a>`-tagger.

## Dypdykk:
I begynnelsen var HTML parsing i funksjonelle språk som Haskell tungvint, men med bibliotek som `hxt`, `tagsoup` og `pandoc` ble det mer tilgjengelig og effektivt. Alternativer til `hxt` inkluderer `tagsoup`, som er mer feiltolerant og kan håndtere dårlig formatert HTML. På implementasjonsnivået bruker `hxt` en kombinasjon av funksjonelle teknikker og parse-transformere for å håndtere HTML/XHTML-strukturer.

## Se Også:
- HXT tutorial: https://wiki.haskell.org/HXT/Practical
- TagSoup's GitHub repo: https://github.com/ndmitchell/tagsoup
- Pandoc hjemmeside: https://pandoc.org
