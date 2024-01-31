---
title:                "Säännöllisten lausekkeiden käyttö"
date:                  2024-01-19
html_title:           "Bash: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
RegExpit eli säännölliset lausekkeet etsivät kaavoja tekstistä. Käytämme niitä tekstidatan siivoamiseen, analysointiin ja validointiin – tehostavat ja automatisoivat monia tehtäviä.

## How to:
Haskellissa säännöllisiä lausekkeita käytetään regex-pakettien kautta. Esimerkiksi, `regex-tdfa` on hyvä ja yleinen valinta.

```Haskell
import Text.Regex.TDFA ((=~))

-- Tarkistetaan, sisältääkö teksti sähköpostiosoitteen
hasEmail :: String -> Bool
hasEmail text = text =~ "[a-zA-Z0-9.-_]+@[a-zA-Z0-9.-_]+\\.[a-zA-Z]{2,}"

main :: IO ()
main = do
  print $ hasEmail "test@example.com" -- True
  print $ hasEmail "ei sähköpostia"   -- False
```

## Deep Dive
RegExpit ovat olleet ohjelmoinnin työkalupakissa jo 1950-luvulta. Alternatiiveja ovat esimerkiksi merkkijonokirjastot ja parserigeneraattorit. Haskellin RegExp-implementaatio nojaa automaatteihin ja ohjelmointikielen laiskaan evaluointiin, mahdollistaen tehokkaiden ja monimutkaisten kuvioiden käsittelyn.

## See Also
- `regex-tdfa` paketin dokumentaatio: https://hackage.haskell.org/package/regex-tdfa
- Real World Haskell, luku 8 (säännölliset lausekkeet): http://book.realworldhaskell.org/read/efficient-file-processing-regular-expressions-and-file-name-matching.html
- Haskell Wiki: https://wiki.haskell.org/Regular_expressions
