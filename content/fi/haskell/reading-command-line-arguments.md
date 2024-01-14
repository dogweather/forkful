---
title:                "Haskell: Komentoriviparametrien lukeminen"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Jos haluat oppia ohjelmoimaan Haskellilla, on tärkeää ymmärtää, kuinka käsitellä komentoriviltä saatuja argumentteja. Tämä on tärkeää silloin, kun haluat ohjailla ohjelmasi käyttäytymistä sen käynnistyksessä.

## Kuinka

Haskellissa komentoriviparametrit luetaan `getArgs`-funktion avulla. Se palauttaa listan `String`-arvoja, jotka vastaavat komentoriviltä annettuja argumentteja. Alla on yksinkertainen esimerkki, jossa luetaan kaksi komentoriviparametria ja tulostetaan ne konsoliin:

```Haskell
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ "Ensimmäinen argumentti: " ++ (args !! 0)
  putStrLn $ "Toinen argumentti: " ++ (args !! 1)
```

**Esimerkkisyöte:**

`runhaskell arguments.hs arg1 arg2`

**Esimerkkilähtö:**

```
Ensimmäinen argumentti: arg1
Toinen argumentti: arg2
```

## Syvällinen sukellus

`getArgs`-funktion sijaan voit käyttää myös `getProgName`-funktiota, joka palauttaa vain ohjelman nimen. Lisäksi voit käyttää `withArgs`-funktiota, jolla voit muuttaa komentoriviparametrien listaa haluamallasi tavalla ennen niiden lukemista.

See Also

- Täältä voit löytää lisätietoa komentoriviparametrien lukemisesta Haskellilla: https://www.schoolofhaskell.com/user/commercial/content/getting-command-line-arguments-in-haskell
- `System.Environment`-moduulin dokumentaatio: https://hackage.haskell.org/package/base-4.11.1.0/docs/System-Environment.html