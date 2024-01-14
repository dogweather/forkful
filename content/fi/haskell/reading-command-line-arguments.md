---
title:    "Haskell: Komentoriviparametrien lukeminen"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Jos olet ohjelmoija, joka käyttää Haskellia, niin sinua kiinnostaa ehkä oppia kuinka lukea komentoriviargumentteja. Tämä taito on hyödyllinen monissa projekteissa, ja siksi on tärkeää oppia kuinka tehdä se oikein. Seuraavassa kerron miten tämä tehdään Haskellissa.

## Kuinka tehdä

Tässä kurssilla käytämme `System.Environment` moduulia, joka tarjoaa funktion `getArgs`. Tämä funktio palauttaa listan merkkijonoja, jotka sisältävät kaikki komentoriviargumentit, jotka annettiin ohjelman suorituksen yhteydessä. Voit kokeilla sitä seuraavalla tavalla:

``` Haskell
import System.Environment 

main = do
    args <- getArgs
    putStrLn "Annetut argumentit:"
    print args
```

Kun suoritat tämän ohjelman komentorivillä käyttäen esimerkiksi komentoa `runhaskell`, saat tulosteen:

```
runhaskell args.hs arg1 arg2 arg3
```
```
Annetut argumentit:
["arg1", "arg2", "arg3"]
```
Kuten näet, `args` muuttuja on lista merkkijonoja, jotka ovat kaikki ne argumentit, jotka annoit suorittaessasi ohjelmaa. Voit käyttää tätä listaa työskennelläksesi argumenttien kanssa koodissasi esimerkiksi käyttäen `elem` funktiota tarkistamaan onko tiettyä argumenttia listassa.

## Syvemmälle

Tarkempaa tietoa `System.Environment` moduulista ja sen sisältämistä funktioista löydät Haskelin virallisesta dokumentaatiosta. Voit myös tutkia erilaisia tapoja käyttää komentoriviargumentteja eri projekteissa.

## Katso myös

- [Haskellin virallinen dokumentaatio](https://www.haskell.org/documentation/)
- [Haskelin `System.Environment` moduuli](https://hackage.haskell.org/package/base-4.14.0.0/docs/System-Environment.html)