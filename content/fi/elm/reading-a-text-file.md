---
title:                "Tiedoston lukeminen"
html_title:           "Elm: Tiedoston lukeminen"
simple_title:         "Tiedoston lukeminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Lukeminen on välttämätöntä monissa ohjelmointitehtävissä, joten on tärkeää tietää, kuinka lukea tiedostoja tehokkaasti. Elm tarjoaa helpon ja tehokkaan tavan lukea teksti- tai CSV-tiedostoja, mikä tekee siitä erinomaisen kielen käytettäväksi tiedonkäsittelyssä.

## Kuinka

```elm
import File
import Csv.Decode as Decode

readFile : String -> Task.Task String (List (List String))
readFile fileName =
    File.asString fileName
        |> Task.map Decode.decodeCsv
```

Koodiesimerkissä käytetään `File`-kirjastoa tekstitiedoston lukemiseen ja `Csv.Decode`-kirjastoa tiedoston sisällön muuntamiseen halutuksi muodoksi. `Task`-tyyppi käytetään tilaa ja virheiden käsittelyä varten.

```elm
case result of
    Task.Ok data ->
        -- Tee jotain data-listan kanssa

    Task.Err error ->
        -- Käsittele virhe
```

Kun tiedosto on luettu, voit käsitellä saatuja tietoja `data`-listan avulla. Jokainen rivi muuttuu omaksi listaksi, joka voi sisältää erilaisia arvoja riippuen tiedoston muodosta. Esimerkiksi jos tiedosto sisältää mittauksia, jokainen rivi voisi sisältää päivämäärän, ajan ja lämpötilan arvot.

## Syvempi sukellus

Oletetaan, että haluamme lukea tiedoston, jossa jokainen rivi sisältää henkilön nimen ja iän välilyönnillä erotettuna. Käytämme tällä kertaa `File.lines`-funktiota, joka palauttaa listan rivejä tiedostosta.

```elm
lowercaseNames : String -> List (String, Int)
lowercaseNames fileName =
    File.lines fileName
        |> List.map String.words
        |> List.map (\[ name, age ] -> (String.toLower name, String.toInt age))
```

Tässä esimerkissä käytettiin `List.map`-funktiota, joka suorittaa annetun muunnoksen jokaiselle listan alkiolle. `String.words`-funktio jakaa annetun merkkijonon välilyöntien kohdalta ja palauttaa listan sanoista.

## Katso myös

- [Elm File -virallinen dokumentaatio](https://guide.elm-lang.org/io/files.html)
- [Elm CSV -koodiesimerkkejä](https://package.elm-lang.org/packages/elm-community/csv/latest/)
- [Elm String -kirjaston dokumentaatio](https://package.elm-lang.org/packages/elm/core/latest/String)