---
title:                "HTML:n jäsentäminen"
date:                  2024-01-20T15:32:04.181978-07:00
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
HTML-parserointi tarkoittaa HTML-dokumentin rakenteen muuttamista ohjelmallisesti käsiteltävään muotoon. Ohjelmoijat tekevät tätä sisällön lukuun, koneelliseen käsittelyyn tai päästäkseen käsiksi sivun tietoihin, joita ei tarjota API:n kautta.

## How to:
```Haskell
-- Install the `tagsoup` library first:
-- cabal install tagsoup

import Text.HTML.TagSoup

-- Etsi linkit HTML:stä
findLinks :: String -> [String]
findLinks html = [href | TagOpen "a" attrs <- parseTags html, ("href", href) <- attrs]

-- Esimerkkikäyttö
main :: IO ()
main = do
  let htmlContent = "<html><body><a href='http://example.com'>Example</a></body></html>"
  print $ findLinks htmlContent
```

Sample output:
```
["http://example.com"]
```

## Deep Dive
HTML-parseroinnin avulla kone voi ymmärtää ja käsitellä web-sivuja, jotka on suunniteltu ihmissilmälle. Haskellin 'TagSoup'-kirjasto on suosittu työkalu tähän, se tarjoaa joustavia funktioita syntaksiltaan viallisenkin HTML:n käsittelyyn. Vaihtoehtoja, kuten 'xml-conduit', on olemassa, mutta ne ovat tiukempia syntaksin suhteen. 'TagSoup' sivuuttaa virheet ja antaa kehittäjän keskittyä sisältöön. Historiallisesti, HTML:n parsiminen on ollut haasteellista johtuen epästandardista koodista ja monimutkaisista sivurakenteista. Haskell tarjoaa työkaluja tämän monimutkaisuuden hahmottamiseen funktio-ohjelmoinnin kautta.

## See Also
- TagSoup library: http://hackage.haskell.org/package/tagsoup
- XML-conduit library: http://hackage.haskell.org/package/xml-conduit
- Functio-ohjelmointi Haskellissa: http://learnyouahaskell.com/chapters
