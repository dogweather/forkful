---
title:                "Työskentely jsonin kanssa"
html_title:           "Elm: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/working-with-json.md"
---

{{< edit_this_page >}}

Mitä ja Miksi?
Työskentely JSON:n kanssa on välttämätöntä monille ohjelmoijille, sillä JSON (JavaScript Object Notation) on yksi yleisimmistä tietojen tallennusmuodoista web-sovelluksissa. Se on erittäin käyttäjäystävällinen ja helppo lukea sekä tulkita, ja siksi sitä käytetään usein tietojen välittämiseen ja tallentamiseen verkkosovelluksissa. JSON mahdollistaa myös tietojen muuttamisen objekteiksi, mikä helpottaa niiden käsittelyä ohjelmoinnissa.

Miten tehdä?
JSON:n käsittely Elm-kielellä on helppoa ja suoraviivaista. Käytä **Json.Decode** -kirjastoa koodissasi. Alla on esimerkki koodinpätkästä, joka hakee ja tulkkaa JSON-tiedoston:

```Elm
import Http
import Json.Decode as Decode


type alias Post = {
    userId : Int,
    id : Int,
    title : String,
    body : String
}


fetchPostList : Cmd Msg
fetchPostList =
    Http.get
        { url = "https://jsonplaceholder.typicode.com/posts",
          expect = Http.expectJson GotPostList (Decode.list postDecoder)
        }


postDecoder : Decode.Decoder Post
postDecoder =
    Decode.map4 Post
        (Decode.field "userId" Decode.int)
        (Decode.field "id" Decode.int)
        (Decode.field "title" Decode.string)
        (Decode.field "body" Decode.string)
```

Tässä esimerkissä haetaan JSON-tiedosto api-palvelusta ja tulkataan se **Post**-objektiksi. **fetchPostList** -funktio käyttää **expectJson** -funktiota, joka määrittää, minkä tyyppinen tulos odotetaan ja miten se tulee tulkata. **postDecoder** -funktio määrittelee, miten JSON-data tulkataan **Post**-objektiksi.

Syvemmällä: JSON:n historiasta, vaihtoehdoista ja toteutuksesta
JSON kehitettiin vuonna 2001 ja se on saavuttanut suuren suosion ohjelmistokehittäjien ja web-sovellusten keskuudessa. Sen yksinkertaiset syntaksit ja helppo käytettävyys ovat tehneet siitä yhden parhaista vaihtoehdoista tietojen tallentamiseen ja välittämiseen verkkosovelluksissa.

Elm-kielessä on myös muita vaihtoehtoja JSON-tiedostojen käsittelyyn, kuten **elm-json** -kirjasto. Kuitenkin, **Json.Decode** on integroitu osaksi Elm-ympäristöä, mikä tekee sen käytöstä erittäin mukavaa ja tehokasta.

On myös tärkeää mainita, että JSON ei tue monimutkaisia tietorakenteita kuten binääripuita tai funktioita. Tämä voi olla rajoittava tekijä joillekin projekteille, mutta useimmiten JSON on riittävä vaihtoehto tietojen käsittelyyn ja säilyttämiseen.

Katso myös
- [Elm's official JSON documentation](https://guide.elm-lang.org/interop/json.html)
- [JSON format specification](https://www.json.org/)