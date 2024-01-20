---
title:                "HTTP-pyynnön lähettäminen"
html_title:           "Bash: HTTP-pyynnön lähettäminen"
simple_title:         "HTTP-pyynnön lähettäminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Mikä & Miksi?

HTTP-pyynnön lähettäminen on tapa, jolla ohjelmamme kommunikoi ulkomaailman kanssa; se hakee tai lähettää tietoa. Ohjelmoijat tekevät tämän hoitaakseen tehtäviä kuten tiedon hakeminen API:sta tai lomakkeen lähettäminen palvelimelle.

# Miten tehdä:

Näin lähetät HTTP-pyynnön Elm-ohjelmointikielellä. Käyttäen `elm/http`-kirjastoa ja `Http.get`-funktiota.

```Elm
import Http
import Json.Decode as Decode

getCharacters = 
  Http.get
    { url = "https://myapi.com/characters"
    , expect = Http.expectJson gotCharacters (Decode.list characterDecoder)
    }

characterDecoder : Decode.Decoder Character
characterDecoder =
  Decode.map3 Character
    (Decode.field "id" Decode.int)
    (Decode.field "name" Decode.string)
    (Decode.field "status" Decode.string)

type Msg = 
  GotCharacters (Result Http.Error (List Character))

gotCharacters : Result Http.Error (List Character) -> Msg
gotCharacters result =
  GotCharacters result
```

Ohjelman suorituksen jälkeen saat ulos listan hahmoja:

```Elm
[
  { "id": 1, "name": "Character1", "status": "active" },
  { "id": 2, "name": "Character2", "status": "inactive" },
  ...
]
```

## Syvä sukellus:

Jotta ymmärrättäisimme paremmin, otetaan pieni katsaus historiaan. HTTP-pyyntöjen lähettäminen on aina ollut tärkeä osa web-kehitystä. Alkuvuosina se oli hankalaa ja sotkuista, mutta modernit ohjelmointikielet, kuten Elm, ovat yksinkertaistaneet prosessia.

Vaikka Elm käyttää `elm/http`-kirjastoa, on olemassa myös muita vaihtoehtoja, kuten `elm-lang/http`, joita voidaan käyttää. Valinta kirjastosta riippuu lopulta omista tarpeistasi.

Lähettäessäsi HTTP-pyynnön, alimman tason yksityiskohdat hoidetaan kirjaston puolesta. Tämä mahdollistaa keskittymisen tärkeämpään - sovelluslogiikkaan.

## Katso myös:

Tarkista nämä linkit lisätietojen saamiseksi:

- Elm:n HTTP-kirjasto: https://package.elm-lang.org/packages/elm/http/latest/
- API-käsittely Elm:ssä: https://korban.net/posts/elm/2018-11-28-working-with-http-and-json-in-elm/
- JSON-koodausteekniikat Elm:ssä: https://package.elm-lang.org/packages/elm/json/latest/Json-Decode