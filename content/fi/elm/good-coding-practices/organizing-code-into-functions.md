---
date: 2024-01-26 01:10:42.118485-07:00
description: "Miten: T\xE4ss\xE4 on palanen Elm-koodia, jossa on yksinkertainen funktio\
  \ k\xE4ytt\xE4j\xE4n tervehtimiseen."
lastmod: '2024-03-13T22:44:56.494265-06:00'
model: gpt-4-1106-preview
summary: "T\xE4ss\xE4 on palanen Elm-koodia, jossa on yksinkertainen funktio k\xE4\
  ytt\xE4j\xE4n tervehtimiseen."
title: "Koodin j\xE4rjest\xE4minen funktioihin"
weight: 18
---

## Miten:
Tässä on palanen Elm-koodia, jossa on yksinkertainen funktio käyttäjän tervehtimiseen:

```Elm
module Main exposing (..)

import Html exposing (text)

greetUser : String -> String
greetUser userName =
    "Hello, " ++ userName ++ "!"

main =
    text (greetUser "Casey")
```

Aja se, ja saat tulosteeksi: "Hello, Casey!"

Entä jos haluat lisätä henkilökohtaisempaa sävyä? Purkkaa lisää toiminnallisuutta!

```Elm
module Main exposing (..)

import Html exposing (text)

greetUser : String -> String -> String
greetUser greeting userName =
    greeting ++ ", " ++ userName ++ "!"

personalGreeting : String -> String
personalGreeting userName =
    greetUser "Howdy" userName

main =
    text (personalGreeting "Casey")
```

Nyt, kun ajat sen: "Howdy, Casey!" Taikuutta? Ei, vain funktiot tekevät tehtävänsä.

## Syväsukellus
Aikoinaan koodi oli usein yksi pitkä toimintojen jono (ajattele spagettikoodia). Se oli painajainen ylläpitää. Sitten rakennoitu ohjelmointi tuli mukaan, ja sen myötä funktiot. Elm, kuten sen funktionaalisen ohjelmoinnin edeltäjät, nojaa vahvasti funktioihin organisoinnissa.

Voit sisäkkäistää funktioita luoden sulkutiloja (closures) tai pitää ne puhtaina yksinkertaistamisen vuoksi. Elm suosii jälkimmäistä: puhtaita funktioita, joilla on hyvin määritellyt sisääntulot ja ulostulot, mikä johtaa helpompaan virheenjäljitykseen ja testaamiseen.

Elm-funktiot voivat myös olla korkeamman asteen, mikä tarkoittaa, että ne voivat vastaanottaa tai palauttaa muita funktioita. Tämä avaa mahdollisuuksien maailman kokoonpanossa. Toisin kuin jotkut muut kielet, Elm ei kuitenkaan tarjoa funktion ylikuormausta; jokaisella funktiolla täytyy olla yksilöllinen nimi.

Lisäksi Elm asettaa vahvan staattisen tyypitysjärjestelmän, joka ei vain tarkista tyyppejä vaan myös päättelee ne, vähentäen kaavamaista koodia.

Verrattaessa vaihtoehtoja, kuten proseduraalista tai oliopohjaista koodin organisointia muihin kieliin, Elmin lähestymistapa korostaa yksinkertaisuutta ja ennustettavuutta. Elm:ssä ei ole olioita tai luokkia. Koodi organisoidaan funktioiden ja moduulien avulla luokkien ja instanssien sijaan.

## Katso Myös
Syventääksesi tietämystäsi, tarkista nämä lähteet:
- Elmin virallinen opas funktioista: https://guide.elm-lang.org/core_language.html
- Elmin pakettidokumentaatio monimutkaisemmille funktioesimerkeille: https://package.elm-lang.org/
- Opi lisää Elmin tyyppijärjestelmästä, joka sopii hienosti yhteen funktion organisoinnin kanssa: https://elm-lang.org/docs/types
