---
title:                "Tekstitiedoston lukeminen."
html_title:           "Elm: Tekstitiedoston lukeminen."
simple_title:         "Tekstitiedoston lukeminen."
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

Mitä & miksi?
Lukeminen tekstityöstä on tärkeä osa ohjelmointia, sillä se antaa meille mahdollisuuden lukea ja käsitellä tiedostojen sisältöä tietokoneella. Näin voimme luoda sovelluksia, jotka voivat lukea ja tallentaa käyttäjän antamia tietoja.

Miten tehdä:
```elm
-- Avataan tiedosto
file <- File.open "tekstityö.txt"
-- Luetaan tiedoston sisältö
contents <- File.read file
-- Tulostetaan sisältö konsoliin
Elm.io.print contents
```
```
Tämä koodi avaa tekstityön tiedoston ja lukee sen sisällön. Tulostamalla sisällön konsoliin voimme nähdä, mitä tiedostossa on.

Syöte:
Hei kaikki!
Tervetuloa lukemaan tekstityötä.
```

Tuloste:
```
Hei kaikki!
Tervetuloa lukemaan tekstityötä.
```

Syventyminen:
Tiedoston lukeminen on tärkeä osa ohjelmointia, sillä aikaisemmin käytettiin pääasiassa tekstityöihin tallennettuja tietoja, kuten käyttäjien tietoja ja asetustiedostoja. Nykyään on olemassa useita muita tapoja lukea ja tallentaa tietoja, kuten tietokantoja ja pilvipalveluita.

Katso myös:
- Elm-opas tiedostojen lukemiseen:
https://guide.elm-lang.org

- Virallinen Elm-sivusto:
https://elm-lang.org