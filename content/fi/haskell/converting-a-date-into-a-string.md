---
title:    "Haskell: Päivämäärän muuttaminen merkkijonoksi"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmoinnin projekteissa on tarve muuntaa päivämäärä merkkijonoksi. Tämä on tärkeää esimerkiksi silloin, kun halutaan tallentaa päivämäärä tietokantaan tai näyttää se käyttäjälle ymmärrettävässä muodossa. Tässä blogikirjoituksessa käsittelemme, miten tämä onnistuu Haskell-ohjelmointikielellä.

## Kuinka

```Haskell
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock (UTCTime, getCurrentTime)
formatDate :: UTCTime -> String
formatDate utc = formatTime defaultTimeLocale "%d.%m.%Y" utc
getCurrentTime >>= putStrLn . formatDate
```

Tässä esimerkissä käytämme Data.Time.Format- ja Data.Time.Clock-kirjastoja saadaksemme UTC-aikaleiman nykyhetkestä. Sitten muunnamme aikaleiman haluttuun merkkijonomuotoon käyttäen formatTime-toimintoa ja defaultTimeLocale-parametria. Lopuksi käytämme getCurrentTime-toimintoa saadaksemme nykyhetken ja tulostamme sen näytölle muunnetussa muodossa.

Esimerkkituloste:

```
08.09.2021
```

## Syväsyventyminen

Yllä olevassa esimerkissä käytimme standardimuotoista "%d.%m.%Y", mutta formatTime-toiminto tukee useita muitakin vaihtoehtoja. Muun muassa voimme käyttää "%F" muodossa, jolloin päivämäärä näytetään ISO 8601 -standardin mukaisessa muodossa "vuosi-kuukausi-päivä".

Lisäksi formatTime-toiminto mahdollistaa myös kellonajan ja aikavyöhykkeen lisäämisen muotoon, jos se on tarpeellista. Tarkemmat tiedot formatTime-toiminnon käytöstä löytyvät Haskellin dokumentaatiosta.

## Katso myös

- [Dokumentaatio Haskellin formatTime-toiminnosta](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html)
- [Haskell-kääntäjä (GHC)](https://www.haskell.org/ghc/)