---
date: 2024-01-26 04:30:17.078859-07:00
description: "Att arbeta med XML inneb\xE4r att tolka, omvandla och generera XML-dokument\
  \ i Elm. Det g\xF6rs f\xF6r att interagera med m\xE5nga webbtj\xE4nster och \xE4\
  ldre system som\u2026"
lastmod: '2024-03-13T22:44:37.853847-06:00'
model: gpt-4-0125-preview
summary: "Att arbeta med XML inneb\xE4r att tolka, omvandla och generera XML-dokument\
  \ i Elm. Det g\xF6rs f\xF6r att interagera med m\xE5nga webbtj\xE4nster och \xE4\
  ldre system som\u2026"
title: Att arbeta med XML
---

{{< edit_this_page >}}

## Vad & Varför?
Att arbeta med XML innebär att tolka, omvandla och generera XML-dokument i Elm. Det görs för att interagera med många webbtjänster och äldre system som använder XML som sitt dataformat.

## Hur man gör:
I Elm hanterar du XML med paketet `elm/xml`. Här är en snabb titt på hur man tolkar ett XML-utdrag:

```Elm
import Xml.Decode exposing (..)
import Xml.Decode.Pipeline exposing (..)

xmlString = """
<book id="123">
    <title>Elm in Action</title>
    <author>Robin Heggelund Hansen</author>
</book>
"""

type alias Bok =
    { id : String
    , title : String
    , author : String
    }

bokDecoder : Decoder Bok
bokDecoder =
    decode Bok
        |> required "id" (attribute "id")
        |> required "titel" (child "title" (content text))
        |> required "författare" (child "author" (content text))

case Xml.Decode.fromString bokDecoder xmlString of
    Ok bok ->
        -- Gör något med den tolkade boken här
        Debug.toString bok

    Err error ->
        -- Hantera fel
        Debug.toString error
```

Exempelutdata, förutsatt att inga fel uppstår:

```Elm
"{ id = \"123\", titel = \"Elm in Action\", författare = \"Robin Heggelund Hansen\" }"
```

## Fördjupning
XML (Extensible Markup Language) har funnits sedan sent 90-tal, en tid då webben var texttung och behovet av ett strukturerat, men flexibelt sätt att bära data var avgörande. På grund av sin utförlighet och komplexitet har XML förlorat mark till JSON. Dock är XML fortfarande utbrett, särskilt i företagsmiljöer eller protokoll som SOAP.

Elms förhållningssätt till XML är funktionellt och typsäkert. Att använda paketet `elm/xml` innebär att omfamna Elms filosofi om explicititet och tillförlitlighet. När det gäller tolkning erbjuder paketet ett utbud av dekodare som du sammanställer för att hantera XML-strukturen.

Jämfört med alternativ som JavaScripts DOMParser eller Pythons ElementTree kan Elms metod verka mer omständlig men säkerställer säkerhet. Inga körtidsfel för saknade fält eller typmismatchningar; om något är fel får du ett kompileringsfel.

`elm/xml`-dekoderfunktionerna baseras på att avbilda XML-noder till Elm-typer. Du bygger dekodare som speglar din datas struktur, vilket säkerställer att din Elm-app hanterar XML lika rigoröst som sina egna interna datastrukturer.

Att generera XML är mindre vanligt i Elm men kan uppnås med `elm/xml`:s motsvarighet `Xml.Encode`.

## Se Också
- Elm-guiden om JSON som också gäller för XML-tankesätt: [https://guide.elm-lang.org/interop/json.html](https://guide.elm-lang.org/interop/json.html)
- XML-standarden av W3C för en djupare förståelse av XML i sig: [https://www.w3.org/XML/](https://www.w3.org/XML/)
