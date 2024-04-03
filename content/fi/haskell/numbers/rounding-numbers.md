---
date: 2024-01-26 03:45:06.538224-07:00
description: "Kuinka: Haskell k\xE4ytt\xE4\xE4 `round`, `ceiling`, `floor` ja `truncate`\
  \ funktioita `Prelude`sta py\xF6ristysoperaatioihin."
lastmod: '2024-03-13T22:44:56.610506-06:00'
model: gpt-4-0125-preview
summary: "Haskell k\xE4ytt\xE4\xE4 `round`, `ceiling`, `floor` ja `truncate` funktioita\
  \ `Prelude`sta py\xF6ristysoperaatioihin."
title: "Numerojen py\xF6rist\xE4minen"
weight: 13
---

## Kuinka:
Haskell käyttää `round`, `ceiling`, `floor` ja `truncate` funktioita `Prelude`sta pyöristysoperaatioihin.

```haskell
import Prelude

main :: IO ()
main = do
  let num = 3.567
  print $ round num    -- 4
  print $ ceiling num  -- 4
  print $ floor num    -- 3
  print $ truncate num -- 3
  
  -- Tietyn desimaalipaikan pyöristäminen ei sisälly Preludeen.
  -- Tässä on räätälöity funktio:
  let roundTo n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)
  print $ roundTo 1 num -- 3.6
```

## Syväsukellus
Historiallisesti pyöristäminen on merkittävää numeerisessa analyysissä ja tietojenkäsittelytieteessä, koska se on välttämätöntä virheiden kertymisen minimoinnissa laskutoimituksissa, erityisesti ennen kuin liukulukuesitykset standardoitiin IEEE 754:n kanssa.

Mihin pyöristetään? `round` vie lähimpään kokonaislukuun—yös tai alas. `ceiling` ja `floor` pyöristävät aina ylös tai alas lähimpään kokonaislukuun, kun taas `truncate` vain pudottaa desimaalipisteet.

Näiden funktioiden vaihtoehtoihin saattaa sisältyä räätälöity logiikka, kuten meidän `roundTo`, tai saatat ottaa käyttöön kirjastoja (kuten Data.Fixed) monimutkaisempien vaatimusten varalle.

Varo odottamattomia tuloksia johtuen siitä, miten Haskell käsittelee puolivälin tapauksia `round`-funktiossa (se pyöristää lähimpään parilliseen numeroon).

## Katso Myös
- Haskell Prelduden dokumentaatio pyöristysfunktioista: https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html
- Haskell Wiki liukulukuaritmetiikasta: https://wiki.haskell.org/Floating_point_arithmetic
- IEEE 754-2008 standardi lisätietoja varten siitä, miten liukulukua käsitellään monissa kielissä: https://ieeexplore.ieee.org/document/4610935
