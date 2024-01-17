---
title:                "Nykyisen päivämäärän saaminen"
html_title:           "Haskell: Nykyisen päivämäärän saaminen"
simple_title:         "Nykyisen päivämäärän saaminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?

Päivämäärän saaminen tarkoittaa nykyisen päivämäärän hakemista ohjelmassa. Tämä on hyödyllistä esimerkiksi päivämäärän mukaisen laskutuksen tai tapahtumien aikaleimojen tallentamisen yhteydessä.

# Kuinka tehdä?

Käytämme ```Haskell getCurrentTime``` -funktiota päivämäärän hankkimiseksi. Päivämäärä muodostuu timestamp-muodossa, joka on määräaika tietyn hetken tallentamiseksi tai laskemiseksi.

```Haskell
import Data.Time.Clock (utctDay, getCurrentTime)

-- Päivämäärän tulostus
main = do
  time <- getCurrentTime
  print $ utctDay time     -- 2021-10-21
```

# Syvemmälle sukellus

Päivämäärän hakeminen on tärkeä osa monia ohjelmia ja monilla ohjelmointikielillä on tähän tarkoitukseen omat funktionsa. Haskellissa ```getCurrentTime``` palauttaa ``