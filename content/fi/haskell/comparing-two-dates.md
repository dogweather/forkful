---
title:                "Kahden päivämäärän vertaaminen"
html_title:           "Bash: Kahden päivämäärän vertaaminen"
simple_title:         "Kahden päivämäärän vertaaminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Vertailu kahden päivämäärän välillä Haskellissa

## Mikä & Miksi?

Päivämäärän vertailu viittaa kahden ajankohdan välisen vertailuprosessiin. Ohjelmoijat tekevät tätä erilaisten aikasidonnaisten ominaisuuksien, kuten vanhentumisajanjaksojen tai aikajanojen, määrittämiseksi ja hallitsemiseksi.

## Miten tehdä:

```Haskell
--Tuo tarvittavat moduulit
import Data.Time.Clock
import Data.Time.Calendar

vertaaPäivämäärät :: Day -> Day -> Ordering
vertaaPäivämäärät päivämäärä1 päivämäärä2 = compare päivämäärä1 päivämäärä2

-- Esimerkkituloste:
-- Siinä siirrämme päivän nykyhetkestä ja vertailemme tämän päivän kanssa.

main :: IO ()
main = do
   nykyhetki <- getCurrentTime
   let tänään = utctDay nykyhetki
   let huomenna = addDays 1 tänään
   print $ vertaaPäivämäärät tänään huomenna  -- tulostaa "LT"

```

Tämä koodi vertailee päivämäärien välillä ja tulostaa "LT" jos ensimmäinen on ennen toista, "GT" jos se on sen jälkeen, ja "EQ" jos ne ovat samana päivänä.

## Syvällisempää tutkintaa:

- Historiallinen konteksti: Haskellin `Data.Time` kirjasto on ollut olemassa 2000-luvun alusta lähtien, mikä on auttanut ohjelmoijia hallitsemaan päivämääriä ja aikaa joustavammin ja tarkemmin kuin aiemmat ratkaisut.

- Vaihtoehtoja: Aiempia ratkaisuja käytettiin `Data.Time.Calendar` ja `Data.Time.Clock` lukujen sijaan, jotka voivat olla epätarkkoja ja sisältää virheitä.

- Toteutuksen yksityiskohdat: Palautetaan `Ordering` tyyppi, joka on osa Haskellin perusrajoja ja se kertoo onko ensimmäinen päivämäärä on ennen ("LT"), jälkeen ("GT") tai sama päivä ("EQ") kuin toinen päivämäärä.

## Katso myös:

- [Data.Time moduuli Haskellin dokumentaatiossa](http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)

- [Haskellin päivämäärät ja ajat - School of Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/date-and-time)

- [Haskellin `Ordering` tyyppi](http://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#t:Ordering)