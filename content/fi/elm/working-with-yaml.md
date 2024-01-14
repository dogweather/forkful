---
title:                "Elm: Yamlin kanssa työskentely"
simple_title:         "Yamlin kanssa työskentely"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi

Miksi työskentelisit YAML:n kanssa? YAML on tietojen tallennusmuoto, joka on erittäin suosittu ohjelmoinnissa ja erityisesti Elm-ohjelmoinnissa. Se on helppo luettava ja kirjoitettava ja sopii hyvin rakenteellisten tietojen tallentamiseen.

## Kuinka tehdä

YAML:n käyttö Elm-ohjelmoinnissa on helppoa ja yksinkertaista. Aloita asentamalla tarvittava kirjasto käyttämällä komentoa `elm install ahstro/elm-yaml` ja tuomalla se ohjelmaasi `import Yaml`. 

Sitten voit lukea YAML-dataa käyttämällä `Yaml.parse` funktiota antamalla sille merkkijonoversion YAML-datasta. Tämän jälkeen voit käsitellä dataa normaalisti Elm:ssä käyttämällä funktioita ja kuvioita.

Esimerkki:

```Elm
fileContent : String
fileContent = """
    name: Jane Doe
    age: 30
    occupation: Developer
    hobbies:
        - Coding
        - Reading
"""

yamlData : Result String Value
yamlData = Yaml.parse fileContent

case yamlData of
    Err msg -> Debug.crash msg
    Ok data ->
        let
            name = data |> Yaml.toList |> List.head |> Maybe.map .data |> Maybe.withDefault "" 
        in
        "Tervetuloa " ++ name
```

Tulostus:
```
"Tervetuloa Jane Doe"
```

## Syvällisempi sukellus

Vaikka YAML on helppo käyttää, on myös tärkeää ymmärtää sen rakennetta ja ominaisuuksia paremmin. YAML käyttää järjestettyä hierarkkista rakennetta, jossa tiedot tallennetaan avain-arvo pareiksi ja sisentämällä eri tasoille. Tämä tekee siitä selvemmän ja luettavamman verrattuna muihin tietojen tallennusmuotoihin.

YAML-tiedostoissa voit myös käyttää kommentteja `#` merkillä ja käyttää muuttujia `&` ja `*` merkeillä. Syvemmän ymmärryksen saavuttamiseksi suosittelemme tutustumaan YAML:n dokumentaatioon ja kokeilemaan erilaisia ​​rakenteita ja vaihtoehtoja koodiesimerkeissä.

## Katso myös

- [Elm-Yaml kirjasto](https://github.com/ahstro/elm-yaml)
- [YAML dokumentaatio](https://yaml.org/)
- [Aloita Elm-ohjelmointi](https://guide.elm-lang.org/)