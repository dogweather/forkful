---
title:                "Komennoriviparametrien lukeminen"
date:                  2024-01-20T17:56:02.282341-07:00
model:                 gpt-4-1106-preview
simple_title:         "Komennoriviparametrien lukeminen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Komennon rivin argumenttien lukeminen mahdollistaa parametrien antamisen ohjelmalle sen käynnistyessä. Ohjelmistokehittäjät käyttävät tätä toimintoa räätälöidäkseen ohjelman toimintaa helposti ja joustavasti.

## How to:
Haskellissa komentorivin argumentit luetaan `System.Environment` kirjaston `getArgs` funktiolla:

```Haskell
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    print args
```

Kun ohjelma ajetaan komentoriviltä esim. `runhaskell script.hs arg1 arg2`, tulostuu:

```Haskell
["arg1", "arg2"]
```

## Deep Dive
Haskellissa komentorivin argumenttien käsittely on suhteellisen yksinkertaista ja suoraviivaista. `System.Environment` on tarjonnut tämän toiminnallisuuden pitkään ja se on tavanomainen tapa käsitellä argumentteja. Vaihtoehtoisesti, voit käyttää kirjastoja kuten `optparse-applicative` monimutkaisempien komentorivin työkalujen rakentamiseen, jotka tarjoavat monipuolisemmat vaihtoehdot ja automaattisen ohjeistuksen.

Implementaation yksityiskohdat ovat melko suoraviivaisia – `getArgs` palauttaa listan merkkijonoja (`[String]`), jotka ovat argumentit ohjelmalle. Järjestelmänriippuvainen osa piiloutuu `System.Environment` kirjaston taakse, joten kehittäjän ei tarvitse murehtia eri käyttöjärjestelmien eroista.

## See Also
Tässä muutamia vinkejä, jos haluat sukeltaa syvemmälle:

- Haskellin dokumentaatio `System.Environment`-moduulista: https://hackage.haskell.org/package/base/docs/System-Environment.html
- `optparse-applicative` kirjaston dokumentaatio: https://hackage.haskell.org/package/optparse-applicative
