---
title:                "Lukeminen komentoriviparametreja"
html_title:           "Haskell: Lukeminen komentoriviparametreja"
simple_title:         "Lukeminen komentoriviparametreja"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Komentoriviparametrien lukeminen on tärkeä osa ohjelmoinnin prosessia. Se tarkoittaa syötteiden vastaanottamista käyttäjältä ohjelman suorituksen aikana. Ohjelmoijat käyttävät tätä toimintoa voidakseen muokata ja mukauttaa ohjelmiaan käyttäjien haluamalla tavalla.

## Miten:

Komentoriviparametrien lukeminen Haskellissa on helppoa. Voit käyttää `getArgs` -funktiota saadaksesi listan käyttäjän syöttämistä parametreista. Alla on esimerkki koodista ja sen antamasta tulosteesta.

```Haskell
import System.Environment

main = do
  args <- getArgs
  putStrLn ("Syötit " ++ (show $ length args) ++ " parametria.")
  putStrLn ("Parametrit olivat: " ++ (show args))
```

Tuloste:
```bash
$ runhaskell args.hs arg1 arg2 arg3
Syötit 3 parametria.
Parametrit olivat: ["arg1", "arg2", "arg3"]
```

## Syväsukellus:

Komentoriviparametrien lukeminen on ollut tärkeä osa ohjelmointikieliä jo vuosien ajan. Se mahdollistaa käyttäjien vuorovaikutuksen ohjelmien kanssa ja antaa heille mahdollisuuden muokata ohjelmien toimintaa syöttämiensä parametrien perusteella.

Vaihtoehtoisia tapoja lukea komentoriviparametreja ovat esimerkiksi `getOpt`, `getOptSimple`, ja `cmdargs` -kirjastot. Nämä tarjoavat laajemman valikoiman toimintoja ja mahdollistavat esimerkiksi parametrien väliset vaihtoehdot ja pakotetut argumentit.

Komentoriviparametrien lukeminen Haskellissa käyttää `getArgs` -funktiota, joka perustuu UNIX-järjestelmän tarjoamiin arg-vetoihin. Tämän takia syötteiden muokkaaminen ennen `getArgs` -funktion kutsumista ei välttämättä tuota haluttuja tuloksia.

## Katso myös:

- [Haskellin virallinen dokumentaatio komentoriviparametrien lukemisesta](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-Environment.html)
- [Tutorial Haskell-komentoriviparametrien lukemisesta](https://www.codecademy.com/learn/learn-haskell/modules/learn-haskell-commands/cheatsheet)