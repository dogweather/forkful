---
title:                "Nykyisen päivämäärän hankkiminen"
html_title:           "Haskell: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Haskell-ohjelmissa nykyisen päivämäärän hankkiminen tarkoittaa järjestelmän kellosta ajankohtaisen päivämäärän ja kellonajan selvittämistä. Ohjelmoijat tekevät näin, kun he haluavat seurata tai merkitä tapahtumia ajassa.

## Kuinka:

Ohjelman kuivaamiseksi, joka hakee nykyisen päivämäärän Haskellilla, voit käyttää Data.Time.kirjastoa. Tässä on yksinkertainen esimerkki:

```Haskell
import Data.Time.Clock
import Data.Time.Calendar

tulostaPaivamäärä :: IO ()
tulostaPaivamäärä = do
    aika <- getCurrentTime
    let paiva = utctDay aika
    putStrLn $ "Tänään on " ++ show paiva
```

Kun ajat tämän ohjelman, saat vastauksen muodossa "Tänään on YYYY-MM-DD", missä YYYY-MM-DD on nykyinen päivämäärä ISO 8601 -muodossa.

## Syvempi Sukellus:

Haskellin tarve hakea järjestelmän päivämäärä ja kellonaika juontavat juurensa ohjelmointikielten varhaiseen historiaan. Järjestelmän aika on aina ollut keskeinen osa päivämäärä- ja kellonaikafunktioita. Haskellin "getCurrentTime"-funktio tekee tämän tietojen hankkimisen vaivattomaksi.

Vaihtoehtoisesti, voit käyttää "Data.Time.LocalTime" -kirjastoa saadaksesi paikallisen ajan UTC:n sijaan. 

Tärkeä detaili ottaa huomioon on, että "getCurrentTime" palauttaa ajan Universal Coordinated Time (UTC) muodossa. Tämä tarkoittaa, että jos haluat näyttää ajankohdan jossa tahansa muussa aikavyöhykkeessä, sinun täytyy muuntaa aika itse.

## Katso Myös:

[Kuinka käyttää Data.Time -kirjastoa](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)

[Haskellin ajan ja päivämäärän käsittelyn perusteet](https://williamyaoh.com/posts/2020-12-20-dates-and-times-in-haskell.html) 

[Haskellin päivämäärä- ja kellonaikatyyppien dokumentaatio](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)