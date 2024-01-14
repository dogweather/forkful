---
title:    "Elm: Lukemassa tekstitiedostoa"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Miksi

Miksi lukisin tekstitiedostoa? Tekstiedostojen lukeminen voi olla hyödyllistä, kun haluat esimerkiksi käsitellä suuria määriä tekstimuotoista dataa tai luoda ohjelmia, jotka ottavat tekstitiedoston sisällön huomioon.

## Kuinka

Seuraavassa on muutamia esimerkkejä siitä, kuinka voit lukea tekstitiedostoa Elm-ohjelmalla.

```elm
import File
import Html

fileContent : String
fileContent =
    File.read "tekstitiedosto.txt"

Html.text fileContent

```

Tässä esimerkissä luomme muuttujan nimeltä `fileContent`, joka sisältää tekstitiedoston sisällön. Sitten tulostamme sen HTML-elementin sisällöksi, jotta voimme näyttää sen käyttäjälle.

```elm
import File
import Array

fileContent : Array String
fileContent =
    File.lines "tekstitiedosto.txt"

```

Tämä esimerkki lukee tekstitiedoston rivit ja tallentaa ne taulukkoon. Tämä on hyödyllistä, jos haluat käsitellä tekstitiedoston sisältöä rivien perusteella.

```elm
import File
import Json.Encode

saveFile : String -> Cmd msg
saveFile fileContent =
    File.write "uusi_tiedosto.txt" (Json.Encode.string fileContent)

```

Tämä esimerkki luo uuden tekstitiedoston ja tallentaa siihen annetun sisällön. Huomaa, että käytämme myös Json.Encode-moduulia muuttaaksemme merkkijonon `fileContent` Json-muotoon.

## Syvempi sukellus

Jos haluat tutustua lisää tekstitiedostojen lukemiseen ja käsittelyyn Elm-ohjelmassa, suosittelemme tutustumaan viralliseen dokumentaatioon. Dokumentaatiossa on kerrottu tarkemmin moduuleista ja funktioista, joita voi käyttää tekstitiedostojen käsittelyyn.

## Katso myös

- [Elm-ohjelmointikielen virallinen dokumentaatio](https://guide.elm-lang.org/)
- [Elm-tiedostokäsittelymoduulin dokumentaatio](https://package.elm-lang.org/packages/elm/file/latest/File)