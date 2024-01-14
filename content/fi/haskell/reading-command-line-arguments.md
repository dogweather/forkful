---
title:                "Haskell: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi: Miksi lukija haluaisi lukea komentoriviparametreja koskevan ohjelmoinnin blogitekstin?

Komentoriviparametrit ovat tärkeitä osia monia Haskell-ohjelmia. Niiden avulla voit antaa ohjelmalle ohjeita ja tietoja muuttamatta koodia. Tämän avulla voit esimerkiksi suorittaa ohjelman eri tavoin eri syötteiden perusteella, mikä tekee siitä joustavamman.

## Miten: Koodiesimerkkejä ja näytä komentoriviparametrien käsittely Haskellissa:

```Haskell
import System.Environment (getArgs)

main = do
	args <- getArgs
	putStrLn ("Terve " ++ args !! 0 ++ " ja " ++ args !! 1 ++ "!")
```

Tässä esimerkissä saamme kaksi argumenttia komentoriviltä ja tulostamme ne yhdessä tervehdyksessä. Voimme kutsua tätä ohjelmaa seuraavasti: `stack runghc hello.hs Maailma Haskell`.

```
Terve Maailma ja Haskell!
```

Toinen tapa käsitellä komentoriviparametreja on käyttää `getArgs` -funktiota ja `head` -funktiota, joka palauttaa ensimmäisen parametrin listasta:

```Haskell
import System.Environment (getArgs)

main = do
	args <- getArgs
	putStrLn ("Ensimmäinen parametri on " ++ head args)
```

Voimme kutsua tätä ohjelmaa seuraavasti: `stack runghc first.hs 1 2 3`.

```
Ensimmäinen parametri on 1
```

## Syvällinen tarkastelu:

Komentoriviparametrien käsittely Haskellissa vaatii `System.Environment`-moduulin tuontia ja `getArgs`-funktion käyttöä. Tämä funktio palauttaa `IO [String]`-tyypin, joka tarkoittaa, että se palauttaa listan merkkijonoja `IO`-monadiin käärittynä. Tämä johtuu siitä, että komentoriviparametrien lukeminen on I/O-toiminto, jotta se voidaan suorittaa ohjelman suoritusjärjestyksen sisällä.

Kun saamme listan argumentteja, voimme käsitellä niitä tutuilla listatoiminnoilla. Esimerkiksi `head`-funktio palauttaa ensimmäisen argumentin, `tail`-funktio kaikki argumentit paitsi ensimmäisen ja `length`-funktio palauttaa argumenttien määrän. Lisäksi komentoriviparametrit tulee aina antaa merkkijonoina, joten niitä voidaan käsitellä tavallisina merkkijonoina.

## Katso myös:

- [Haskellin virallinen dokumentaatio komentoriviparametreista](https://hackage.haskell.org/package/base-4.14.0.0/docs/System-Environment.html)
- [Haskell opetusohjelma: Komentoriviparametrien lukeminen](https://www.haskell.org/tutorial/io.html#command-line-arguments)
- [Learn You a Haskell for Great Good!: Komentoriviparametrit](http://learnyouahaskell.com/input-and-output#command-line-arguments)