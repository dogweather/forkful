---
title:    "Haskell: Komentoriviparametrien lukeminen"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Miksi lukea komentorivin argumentteja?

Komentoriviltä saa usein tärkeitä lisätietoja, joita tarvitaan ohjelman suorittamiseen. Varmistamalla, että ohjelma hyödyntää oikeita argumentteja, voit välttyä virheiltä ja säästää aikaa.

## Miten lukea komentorivin argumentteja?

Käytä *getArgs* -funktiota, joka palauttaa listan komentorivin argumenteista. Voit sitten käyttää *head* tai *!!* -toimintoa poimimaan haluamasi argumentin.

```Haskell
import System.Environment

main = do
    args <- getArgs
    let arg1 = head args
    let arg2 = args !! 1
    putStrLn ("Ensimmäinen argumentti: " ++ arg1)
    putStrLn ("Toinen argumentti: " ++ arg2)
```

Esimerkki inputista ja outputista:

```
$ runhaskell argumentit.hs ensimmäinen toinen

Ensimmäinen argumentti: ensimmäinen
Toinen argumentti: toinen
```

## Syvemmälle komentorivin argumentteihin

Voit myös käyttää *System.Environment* -pakettia saadaksesi lisätietoja argumenteista. Esimerkiksi *withArgs* funktiolla voit määrittää omat argumenttisi ja ajaa ohjelman näillä argumenteilla.

```Haskell
import System.Environment

main = do
    withArgs ["Apple", "Banana"] $ do
        args <- getArgs
        putStrLn ("Ensimmäinen argumentti: " ++ head args)
        putStrLn ("Toinen argumentti: " ++ args !! 1)
```

Output:

```
Ensimmäinen argumentti: Apple
Toinen argumentti: Banana
```

## Katso myös

- [Haskellin virallinen dokumentaatio](https://www.haskell.org/documentation/)
- [Haskell -ohjelmointikielen perusteet](https://www.tutorialspoint.com/haskell/)
- [Lisätietoja *System.Environment* -paketista](https://hackage.haskell.org/package/base-4.12.0.0/docs/System-Environment.html)