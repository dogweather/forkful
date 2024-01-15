---
title:                "HTML:n jäsentäminen"
html_title:           "Haskell: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi oppia parsimaan HTML:ää? Yksinkertaisesti sanottuna, kyseessä on tärkeä taito web-kehittäjille. HTML on yksi tärkeimmistä kielistä, jota käytetään verkkosivujen luomisessa ja sen ymmärtäminen auttaa parantamaan ohjelmointitaitoja.

## Miten

Parsiminen tarkoittaa tekstin pilkkomista osiin ja niiden analysointia. Haskellilla pystytään suorittamaan tätä hyvin helposti käyttäen pakettia "html-conduit". Tässä yksinkertainen esimerkki, joka hakee otsikon ja linkin kaikista `<h2>` elementeistä:

```haskell
import Text.HTML.Conduit
import Data.Conduit
import qualified Data.Conduit.List as CL

main :: IO ()
main = do
  sourceFile "example.html" $$ element "h2" =$ CL.mapM_ print
```

Tulostus:

```haskell
"<h2>Ensimmäinen otsikko</h2>"
"<h2>Toinen otsikko</h2>"
"<h2>Kolmas otsikko</h2>"
```

## Syväsukellus

HTML:n parsiminen voi mennä syvemmälle kuin pelkkien elementtien hakuun. Voit myös suorittaa monimutkaisempia toimintoja, kuten tiettyjen attribuuttien hakuja tai tietojen poimimista taulukoista. Tässä esimerkki, jossa haetaan kaikki otsikot ja niiden attribuutit:

```haskell
sourceFileLBS "example.html" $$ fromDocument =$= element "h2" =$=
  CL.map attr =$= CL.mapM_ print

attr (EventBeginElement name attrs) = nameLocalName name ++ " : " ++ show attrs
attr _ = ""
```

Tulostus:

```haskell
"Ensimmäinen otsikko : [Attr {attrKey = "class", attrVal = "title"},Attr {attrKey = "id", attrVal = "first"}]"
"Toinen otsikko : []"
"Kolmas otsikko : [Attr {attrKey = "id", attrVal = "third"}]"
```

## Katso myös

- [hackage.haskell.org/package/html-conduit](https://hackage.haskell.org/package/html-conduit)
- [learnyouahaskell.com/chapters](http://learnyouahaskell.com/chapters)