---
title:                "Werken met XML"
aliases:
- /nl/elm/working-with-xml/
date:                  2024-01-28T22:11:55.375759-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/elm/working-with-xml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Werken met XML betekent het parseren, transformeren en genereren van XML-documenten in Elm. Dit wordt gedaan om te interageren met veel webdiensten en verouderde systemen die XML gebruiken als hun gegevensformaat.

## Hoe:
In Elm ga je om met XML met behulp van het `elm/xml` pakket. Hier is een snelle blik op het parseren van een XML-snippet:

```Elm
import Xml.Decode exposing (..)
import Xml.Decode.Pipeline exposing (..)

xmlString = """
<book id="123">
    <title>Elm in Actie</title>
    <author>Robin Heggelund Hansen</author>
</book>
"""

type alias Boek =
    { id : String
    , titel : String
    , auteur : String
    }

boekDecoder : Decoder Boek
boekDecoder =
    decode Boek
        |> required "id" (attribute "id")
        |> required "titel" (child "title" (content text))
        |> required "auteur" (child "author" (content text))

case Xml.Decode.fromString boekDecoder xmlString of
    Ok boek ->
        -- Doe hier iets met het gedecodeerde boek
        Debug.toString boek

    Err error ->
        -- Fouten afhandelen
        Debug.toString error
```

Voorbeelduitvoer, uitgaande van geen fouten:

```Elm
"{ id = \"123\", titel = \"Elm in Actie\", auteur = \"Robin Heggelund Hansen\" }"
```

## Diepere Duik
XML (eXtensible Markup Language) bestaat sinds de late jaren '90, een periode waarin het web tekstzwaar was en de noodzaak voor een gestructureerde, maar flexibele manier om gegevens te dragen cruciaal was. Vanwege zijn langdradigheid en complexiteit heeft XML wat terrein verloren aan JSON. Echter, XML is nog steeds overheersend, vooral in zakelijke omgevingen of protocollen zoals SOAP.

Elms benadering van XML is functioneel en type-veilig. Het gebruik van het `elm/xml` pakket betekent het omarmen van de Elm-filosofie van expliciet zijn en betrouwbaarheid. Wat betreft het parseren biedt het pakket een reeks decoders die je samenstelt om de XML-structuur te hanteren.

In vergelijking met alternatieven zoals JavaScripts DOMParser of Pythons ElementTree, kan Elms methode omslachtiger lijken maar zorgt voor veiligheid. Geen runtime-uitzonderingen voor ontbrekende velden of type mismatches; als er iets fout is, krijg je een compilatiefout.

De decodeerfuncties van `elm/xml` zijn gebaseerd op het in kaart brengen van XML-knooppunten naar Elm-types. Je bouwt decoders die de vorm van je gegevens weerspiegelen, zodat je Elm-app XML net zo rigoureus behandelt als zijn eigen interne gegevensstructuren.

Het genereren van XML komt minder vaak voor in Elm, maar kan worden bereikt met het tegenhanger van `elm/xml` `Xml.Encode`.

## Zie Ook
- Elm-gids over JSON die ook van toepassing is op de XML-mindset: [https://guide.elm-lang.org/interop/json.html](https://guide.elm-lang.org/interop/json.html)
- XML-standaard door W3C voor een dieper begrip van XML zelf: [https://www.w3.org/XML/](https://www.w3.org/XML/)
