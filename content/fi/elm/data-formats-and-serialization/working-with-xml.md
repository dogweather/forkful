---
title:                "XML:n käsittely"
aliases:
- /fi/elm/working-with-xml/
date:                  2024-01-26T04:30:19.095585-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML:n käsittely"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/working-with-xml.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Työskentely XML:n kanssa tarkoittaa XML-dokumenttien jäsentämistä, muuntamista ja tuottamista Elm:ssä. Syy tähän on vuorovaikutuksessa monien web-palveluiden ja perintöjärjestelmien kanssa, jotka käyttävät XML:ää datamuotonaan.

## Kuinka:
Elm:ssä käsittelet XML:ää käyttämällä `elm/xml` -pakettia. Tässä pikakatsaus XML-katkelman jäsentämiseen:

```Elm
import Xml.Decode exposing (..)
import Xml.Decode.Pipeline exposing (..)

xmlString = """
<book id="123">
    <title>Elm in Action</title>
    <author>Robin Heggelund Hansen</author>
</book>
"""

type alias Book =
    { id : String
    , title : String
    , author : String
    }

bookDecoder : Decoder Book
bookDecoder =
    decode Book
        |> required "id" (attribute "id")
        |> required "title" (child "title" (content text))
        |> required "author" (child "author" (content text))

case Xml.Decode.fromString bookDecoder xmlString of
    Ok book ->
        -- Tee jotain jäsentämäsi kirjan kanssa täällä
        Debug.toString book

    Err error ->
        -- Käsittele virheet
        Debug.toString error
```

Näyte tulosteesta, olettaen että ei virheitä:

```Elm
"{ id = \"123\", title = \"Elm in Action\", author = \"Robin Heggelund Hansen\" }"
```

## Syväsukellus
XML (laajennettavissa oleva merkkauskieli) on ollut olemassa 90-luvun lopulta lähtien, aikana, jolloin web oli tekstitiheä ja tarve rakenteelliselle, mutta joustavalle datan kantamisen tavalle oli kriittinen. Verbositeetin ja monimutkaisuuden vuoksi XML on menettänyt hieman maata JSON:lle. XML on kuitenkin edelleen vallitseva, erityisesti yritysympäristöissä tai protokollissa kuten SOAP.

Elm:n lähestyminen XML:ään on toiminnallinen ja tyyppiturvallinen. `elm/xml` -paketin käyttäminen tarkoittaa Elm:n filosofian omaksumista; erityisesti selkeyden ja luotettavuuden. Jäsentämisen osalta paketti tarjoaa joukon dekoodereita, joita voit yhdistellä käsittelemään XML-rakennetta.

Verrattuna vaihtoehtoihin, kuten JavaScriptin DOMParser tai Pythonin ElementTree, Elm:n menetelmä saattaa vaikuttaa verbosilta, mutta se takaa turvallisuuden. Ei ajonaikaisia poikkeuksia puuttuvista kentistä tai tyyppivirheistä; jos jokin on vialla, saat käännösaikaisen virheen.

`elm/xml` paketin dekoodausfunktiot perustuvat XML-noodien kartoittamiseen Elm-tyypeiksi. Rakennat dekoodereita, jotka peilaavat datasi muotoa, varmistaen, että Elm-sovelluksesi käsittelee XML:ää yhtä perusteellisesti kuin omia sisäisiä data-rakenteitaan.

XML:n tuottaminen Elm:ssä on vähemmän yleistä, mutta se on mahdollista `elm/xml` paketin vastapuolen `Xml.Encode` avulla.

## Katso Myös
- Elm-oppaan JSON, joka pätee myös XML-ajattelutapaan: [https://guide.elm-lang.org/interop/json.html](https://guide.elm-lang.org/interop/json.html)
- XML-standardi W3C:ltä XML:n syvemmän ymmärtämisen saavuttamiseksi: [https://www.w3.org/XML/](https://www.w3.org/XML/)
