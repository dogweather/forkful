---
title:                "Päivämäärän jäsentäminen merkkijonosta"
html_title:           "Bash: Päivämäärän jäsentäminen merkkijonosta"
simple_title:         "Päivämäärän jäsentäminen merkkijonosta"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Aikaleimamuotoilu on prosessi, jossa otetaan merkkijono, joka kuvaa päivämäärää, ja muutetaan se koneen ymmärtämään päivämäärämuotoon, esimerkiksi siihen muotoon, että sitä voidaan vertailla muihin päivämääriin. Ohjelmoijat tekevät sen, kun heidän tarvitsee käsitellä päivämääriä, jotka ovat tulleet tiedostosta, verkkolomakkeesta tai muusta liikkuvasta lähteestä.

## Näin tehdään:

```Haskell
import Data.Time

parseDate :: String -> IO UTCTime
parseDate input = parseTimeM True defaultTimeLocale "%Y-%m-%d" input :: IO UTCTime

main = do
    parsedDate <- parseDate "2020-07-01"
    print parsedDate
```

Tämä ohjelma ottaa sisään päivämäärämerkkijonon huipputason funktiolle `parseDate` ja palauttaa tulokset keskitason päivämääränä.

## Syvemmälle

Haskellin aikapäivämäärän parseaushistoria perustuu POSIX-tyylisiin aikatoimintoihin ja funktioihin, jotka ovat jo pitkään olleet ohjelmistokehityksen vakiotoiminnot. Haskellissa on myös useita vaihtoehtoisia tapoja päivämäärän jäsennys, esim. Parsec-kirjasto.

`parseTimeM`-toiminto tekee kaiken raskaan työn. Sille annetaan lokalisointitiedot - tässä tapauksessa `defaultTimeLocale`, joka kertoo kuinka voisit analysoida merkkijonot, sillä on oletetta, että olemme käsittelemässä ISO 8601 -muotoisia päivämääriä.

## Katso myös

- [Data.Time-moduuli Hoogle-ohjelmassa](https://hoogle.haskell.org/?hoogle=Data.Time)

- [Parsec-kirjasto Hackage-sivustolla](https://hackage.haskell.org/package/parsec)