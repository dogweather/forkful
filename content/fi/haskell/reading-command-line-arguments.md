---
title:                "Komentoriviparametrien lukeminen"
html_title:           "Haskell: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

Haskell - mitä hyötyä komentoriviparametrien lukemisella on?

Monet kokeneet ohjelmoijat voivat hyötyä komentoriviparametrien lukemisesta, sillä se voi säästää aikaa ja vaivaa ohjelmia kirjoitettaessa ja testattaessa.

## Miten tehdä se?

Komentoriviparametrien lukeminen Haskellissa on helppoa. Tarvitset vain muutaman rivin koodia ja käsittelykoodin - ei muuta.

```Haskell
import System.Environment

main = do
    args <- getArgs
    putStrLn $ "Komentoriviparametrit: " ++ show args
```

Kun ajat tätä esimerkkiä seuraavasti `runhaskell read_args.hs hello world`, saat tuloksen `Komentoriviparametrit: ["hello", "world"]`.

## Syvempi sukellus

Voit huomata, että `getArgs` palauttaa listan `String`-arvoja. Jos haluat lukea parametrin numeroarvoja, voit käyttää `read`-funktiota muuntamalla `String`-arvon haluttuun tyyppiin. Esimerkiksi, jos haluat lukea kokonaisluvun, voit käyttää seuraavaa koodia:

```Haskell
import System.Environment

main = do
    args <- getArgs
    putStrLn ("Lukemasi luku on " ++ show (read (head args) :: Int))
```

Joten kun ajat tätä esimerkkiä `runhaskell read_args.hs 5`, saat tuloksen `Lukemasi luku on 5`.

## Katso myös

- [Haskellin dokumentaatio: System.Environment](https://www.haskell.org/onlinereport/haskell2010/haskellch26.html#x33-33000026)
- [Hakukonemyynti: Korkeamman asteen summat](http://hackage.haskell.org/package/base/docs/src/GHC.Float.html#sum)