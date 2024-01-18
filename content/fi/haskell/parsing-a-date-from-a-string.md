---
title:                "Päivämäärän erottaminen merkkijonosta"
html_title:           "Haskell: Päivämäärän erottaminen merkkijonosta"
simple_title:         "Päivämäärän erottaminen merkkijonosta"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Päivämäärän parsiminen merkkijonosta tarkoittaa päivämäärän erottamista merkkijonosta ja muuntamista tietokoneen ymmärtämään muotoon. Tätä tehdään yleensä ohjelmoinnissa, kun halutaan käsitellä päivämääriä ja vertailla niitä toisiinsa.

## Kuinka tehdä?

Merkkijonosta parsimiseen on useita tapoja, mutta Haskell tarjoaa kätevät työkalut tähän tarkoitukseen. Alla on esimerkkejä, miten voit parsia päivämäärän merkkijonosta Haskellilla:

```Haskell
parseDate "12.12.2021" -- palauttaa Just 2021-12-12
```

```Haskell
parseTimeM True defaultTimeLocale "%d.%m.%Y" "12.12.2021" :: Maybe Day -- palauttaa Just 2021-12-12
```

```Haskell
parseTimeM True defaultTimeLocale "%d.%m.%Y" "12.12.2021" :: Maybe UTCTime -- palauttaa Just 2021-12-12 00:00:00 UTC
```

## Syvemmälle

Päivämäärän parsiminen merkkijonosta on tullut tärkeäksi erityisesti tietokoneohjelmoinnissa, sillä monet ohjelmistot ja sovellukset käsittelevät päivämääriä ja halutaan, että ne ovat yhteensopivia keskenään eri formaateilla. On myös mahdollista parsia päivämäärä useilla eri tavoilla riippuen tarpeesta. Esimerkiksi Haskellissa on tarjolla valmiita funktioita ja työkaluja, mutta myös itse voi luoda oman parsimistoiminnon.

## Katso myös

- [Haskellin virallinen dokumentaatio päivämäärien parsimiseen](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html)
- [Stack Overflow: How to parse date string in Haskell?](https://stackoverflow.com/questions/14101283/how-to-parse-date-string-in-haskell)