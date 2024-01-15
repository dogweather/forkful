---
title:                "Väliaikaisen tiedoston luominen"
html_title:           "Elm: Väliaikaisen tiedoston luominen"
simple_title:         "Väliaikaisen tiedoston luominen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi

Temporary filejen luominen voi olla hyödyllistä, kun haluat tallentaa väliaikaisia tietoja, kuten käyttäjän syötteitä tai muuttujia, jotka eivät ole oleellisia pysyvässä tallennuksessa.

## Miten

```Elm
import File
import String

-- Luo temporary file ja tallenna siihen dataa
createTemporaryFile : Task x String
createTemporaryFile =
  File.temporarily "elm-temp" "temp.txt" |> Task.andThen
    (\path ->
      let
        data = "Tämä on väliaikainen data."
      in
        File.write path data
    )

-- Tulostetaan createdTemporaryFile tehtävän tulos
main : Program x String
main = 
  Task.attempt Debug.log
    createTemporaryFile
```

Tulostus:

```
Ok "temp.txt"
```

Huomaa, että temporary file on luotu ja siihen on tallennettu meidän määrittelemä data.

## Syvempi sukellus

Temporary filejen luomisessa on hyvä ottaa huomioon muutamia asioita. Ensinnäkin, temporary filejen tulee olla nimeltään uniikkeja, jotta ne eivät sekoitu muihin tiedostoihin. Elm tarjoaa tähän File.temporarily-funktion, joka generoi automaattisesti uniikin tiedostonimen.

Toisekseen, on tärkeää huomioida turvallisuus. Temporary filejä ei tulisi käyttää pysyvässä tallennuksessa, sillä ne voivat olla alttiita tietoturvauhille. Sen sijaan, niitä tulisi käyttää vain väliaikaiseen tallennukseen ja tuhota käytön jälkeen.

## Katso myös

- Elm:n viralliset dokumentaatiot: https://elm-lang.org/docs
- Temporary filejen käyttöohjeet: https://www.codota.com/code/javascript/functions/fs/createTempFile