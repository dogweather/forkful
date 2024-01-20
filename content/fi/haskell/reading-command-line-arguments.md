---
title:                "Komentorivin argumenttien lukeminen"
html_title:           "Bash: Komentorivin argumenttien lukeminen"
simple_title:         "Komentorivin argumenttien lukeminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?

Komentorivin argumentit ovat ohjelman suorituksen aikana käyttäjän antamat syötteet. Ne mahdollistavat ohjelman suorituksen mukauttamisen lennosta. 

## Kuinka: 

Haskellissa voit lukea komentorivin argumentteja ```getArgs```-funktion avulla. Tässä on esimerkki:

```Haskell
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    print args
```

Tämä koodi tulostaa kaikki käyttäjän syöttämät argumentit. Esimerkkejä ajoista:

```Shell
$ runhaskell example.hs argumentti1 argumentti2
["argumentti1","argumentti2"]
```

## Syvempi sukellus

```getArgs```-funktio on osa ```System.Environment```-kirjastoa, joka on ollut osa Haskell-standardia 1.3-versiosta lähtien. Vaikka funktio on suoraviivainen ja helppokäyttöinen, sen rinnalle on kehitetty myös monimutkaisempia ratkaisuja, kuten ```optparse-applicative```-kirjasto. ```optparse-applicative``` tukee monimutkaisten komentorivikomentojen määrittelyä.

Käytännön tasolla ```getArgs``` toteutetaan siten, että se hakee listan argumentteja suoraan komentotulkin (shellin) ympäristöstä. On hyvä muistaa, että tämä lista ei sisällä itse ohjelman nimeä, jos tarvitset sen, voit käyttää ```getProgName```-funktiota.

## Katso myös

- ```System.Environment``` [dokumentaatio](http://hackage.haskell.org/package/base/docs/System-Environment.html)
- ```optparse-applicative``` [dokumentaatio](https://hackage.haskell.org/package/optparse-applicative) 
- Yleisemmin ohjelmointi Haskellilla [Learn You a Haskell](http://learnyouahaskell.com/chapters) 
- Kattava keskustelu Haskellin komentoriviparametreistä [StackOverflow](https://stackoverflow.com/questions/10129381/cli-argument-parsing-in-haskell)