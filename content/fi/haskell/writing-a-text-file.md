---
title:                "Tekstitiedoston kirjoittaminen"
html_title:           "Haskell: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tekstitiedoston kirjoittaminen on tärkeä osa monia ohjelmointitehtäviä. Se voi olla hyödyllistä tallentaa tietoa, luoda tietokantoja tai vain tarkistaa ohjelman toimivuus. Haskellin avulla voit helposti luoda ja hallita tekstitiedostoja tehokkaasti.

## Kuinka

Creating a text file in Haskell is simple and straightforward. All you need to do is import the necessary libraries and use the `writeFile` function.

```Haskell
import System.IO

main = do
    let content = "Tervetuloa lukemaan tätä artikkelia!"
    writeFile "tervetuloa.txt" content
```

The code above creates a file called "tervetuloa.txt" and writes the specified content to it. The `writeFile` function takes two parameters: the file name and the content to be written.

To append more content to an existing file, we can use the `appendFile` function instead.

```Haskell
main = do
    let moreContent = ", toivottavasti saat siitä jotain hyödyllistä irti!"
    appendFile "tervetuloa.txt" moreContent
```

This will add the content ", toivottavasti saat siitä jotain hyödyllistä irti!" to the end of the file "tervetuloa.txt".

## Syväsukellus

Tekstitiedoston kirjoittamisen lisäksi Haskellilla on mahdollista myös lukea tietoa tekstitiedostoista. Tätä varten käytetään `readFile` funktiota, joka lukee tiedoston sisällön ja palauttaa sen merkkijonona.

```Haskell
main = do
    content <- readFile "tervetuloa.txt"
    putStrLn content
```

Yllä oleva koodi tulostaa teksti-tiedoston sisällön konsoliin. Lisäksi voit myös käyttää `openFile` funktiota, joka antaa sinulle enemmän hallintamahdollisuuksia tiedoston käsittelyssä.

## Katso myös

- [Haskellin virallinen dokumentaatio](https://www.haskell.org/documentation/)
- [Haskellin kirjoitusohjeet ja käytännöt] (https://wiki.haskell.org/How_to_write_a_Haskell_program)
- [Haskellin perustietokurssi] (https://learnxinyminutes.com/docs/fi-fi/haskell-fi/)