---
title:                "Päivämäärän muuntaminen merkkijonoksi"
html_title:           "Haskell: Päivämäärän muuntaminen merkkijonoksi"
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Päivämäärän muuttaminen merkkijonoksi on tärkeä osa ohjelmointia, jossa päivämäärät ovat usein käytössä. Ohjelmoijat tekevät tämän muuntamisen helpottaakseen päivämäärän käyttöä koodissa ja tulostaa sitä tarvittaessa.

## Kuinka?
Käytä ```Haskell``` -koodilohkoa muuntaaksesi päivämäärän merkkijonoksi ```show``` funktiolla:

```Haskell
show (day ++ "." ++ month ++ "." ++ year)
```

Esimerkki tuotoksesta:
```
"10.12.2020"
```

```Haskell``` tarjoaa myös valmiin moduulin ```Data.Time.Format```, joka mahdollistaa päivämäärän muuttamisen halutun muodon mukaan. Esimerkiksi:

```Haskell
formatTime defaultTimeLocale "%A, %e. %B %Y" date
```

Palauttaisi seuraavan stringin:

```
"Torstai, 10. Joulukuu 2020"
```

## Syväsukellus
Päivämäärän muuntamisen merkkijonoksi tarve juontaa juurensa siihen, että päivämäärät ovat usein käyttöliittymissä tai tallennettavissa tietokantoihin erilaisina muodoina (esim. ISO 8601, Unix-aikaleima jne.). Tähän voidaan käyttää myös muita lähestymistapoja, kuten tietokannan ominaisuuksia, mutta merkkijonoksi muuntaminen on usein helpoin vaihtoehto pienen datan käsittelyssä.

## Katso myös
- [```Data.Time.Format``` dokumentaatio] (https://hackage.haskell.org/package/time/docs/Data-Time-Format.html)
- [Päivämäärän säätö kirjasto ```timeform```] (http://hackage.haskell.org/package/timeform)