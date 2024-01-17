---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Haskell: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Tekstin etsiminen ja korvaaminen on tärkeä osa ohjelmointia, sillä se mahdollistaa tietyn merkkijonon löytämisen ja korvaamisen toisella. Tämä voi olla hyödyllistä esimerkiksi virheiden korjaamisessa tai halutun tiedon etsimisessä suuresta tietokannasta.

## Kuinka tehdä?
Seuraavassa on joitakin esimerkkejä siitä, kuinka voit etsiä ja korvata tekstiä Haskell-kielellä:

```Haskell
-- Etsi merkkijono "kuinka" ja korvaa se "miten"
replace "kuinka" "miten" "Kuinka tehdä hyvä kahvi?"
-- output: "Miten tehdä hyvä kahvi?"

-- Etsi ja korvaa kaikki isot kirjaimet pieniksi kirjaimiksi
map toLower "HELLO WORLD"
-- output: "hello world"

-- Etsi ja korvaa kaikki numerot nollilla
map (\x -> if isDigit x then '0' else x) "He1llo 2Wor3ld"
-- output: "He0llo 0Wor0ld"
```

## Syväsukellus
Historiallisesti tekstien etsiminen ja korvaaminen on ollut yleinen tapa tehdä muutoksia ohjelmakoodiin ennen kuin versionhallintatyökalut tulivat käyttöön. Nykyään on olemassa myös muita vaihtoehtoja, kuten säännölliset lausekkeet, joita voi käyttää tekstien etsimiseen ja korvaamiseen monimutkaisemmissa tapauksissa. Haskellissa tekstien etsimiseen ja korvaamiseen on tarjolla myös useita kirjastoja, kuten "regex" ja "replace-megaparsec".

## Katso myös
- [Haskellin viralliset ohjeet](https://www.haskell.org/documentation/)
- [Säännölliset lausekkeet](https://regexr.com/)