---
title:                "Tekstitiedoston kirjoittaminen"
html_title:           "Elm: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?

Tekstitiedoston kirjoittaminen on yksinkertaista - vain avaat tekstitiedostosi selaimesta ja kirjoitat siihen haluamasi tekstiä. Ohjelmoijat tekevät tätä siksi, että tekstitiedostoja voidaan käyttää tallentamaan tietoa ja muokkaamaan sitä myöhemmin.

# Miten:

Elmilla tekstitiedoston kirjoittaminen on helppoa:
```
import File
import Html exposing (text)

writeToFile : String -> Cmd msg
writeToFile content =
    File.write "tekstitiedosto.txt" content
        |> Task.perform (err -> text "Error writing to file")
        (\_ -> text "Text file successfully written!")
```

Suorittaessasi tätä koodia tekstitiedosto "tekstitiedosto.txt" luodaan samaan kansioon kuin koodisi, ja siihen tallennetaan haluamasi sisältö.

# Syväsukellus:

Tekstitiedoston kirjoittaminen on tärkeä osa ohjelmointia, sillä tiedostot voivat tallentaa pysyvästi tietoa, jota voidaan käyttää myöhemmin. Jotkut ohjelmoijat käyttävät myös erilaisia ​​tietokantoja tallentaakseen tietoa, mutta tekstitiedosto on edelleen yleinen ja helppo tapa tallentaa ja käsitellä tietoa.

# Katso myös:

Lisätietoa tekstitiedoston kirjoittamisesta Elmilla löydät viralliselta sivulta: 
https://package.elm-lang.org/packages/elm/file/latest/

Tutustu myös muihin tapoihin tallentaa tietoa Elmilla: 
https://guide.elm-lang.org/effects/file.html