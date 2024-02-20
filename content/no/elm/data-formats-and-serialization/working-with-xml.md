---
date: 2024-01-26 04:30:19.645924-07:00
description: "\xC5 jobbe med XML inneb\xE6rer parsing, transformasjon og generering\
  \ av XML-dokumenter i Elm. Det gj\xF8res for \xE5 samhandle med mange webtjenester\
  \ og eldre\u2026"
lastmod: 2024-02-19 22:04:59.979565
model: gpt-4-0125-preview
summary: "\xC5 jobbe med XML inneb\xE6rer parsing, transformasjon og generering av\
  \ XML-dokumenter i Elm. Det gj\xF8res for \xE5 samhandle med mange webtjenester\
  \ og eldre\u2026"
title: "\xC5 jobbe med XML"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å jobbe med XML innebærer parsing, transformasjon og generering av XML-dokumenter i Elm. Det gjøres for å samhandle med mange webtjenester og eldre systemer som bruker XML som deres dataformat.

## Hvordan:
I Elm, håndterer du XML ved hjelp av `elm/xml`-pakken. Her er en rask titt på parsing av et XML-utdrag:

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
        -- Gjør noe med den dekodede boken her
        Debug.toString book

    Err error ->
        -- Håndter feil
        Debug.toString error
```

Eksempel på utdata, med forutsetning om ingen feil:

```Elm
"{ id = \"123\", title = \"Elm in Action\", author = \"Robin Heggelund Hansen\" }"
```

## Dypdykk
XML (eXtensible Markup Language) har eksistert siden slutten av 90-tallet, en tid da internett var teksttungt og behovet for en strukturert, men fleksibel måte å bære data på var avgjørende. På grunn av verbositet og kompleksitet, har XML mistet noe terreng til JSON. Men, XML er fremdeles utbredt, spesielt i bedriftsmiljøer eller protokoller som SOAP.

Elms tilnærming til XML er funksjonell og typesikker. Å bruke `elm/xml`-pakken betyr å omfavne Elms filosofi om eksplisitthet og pålitelighet. Når det kommer til parsing, tilbyr pakken en rekke dekodere som du komponerer for å håndtere XML-strukturen.

Sammenlignet med alternativer som JavaScripts DOMParser eller Pythons ElementTree, kan Elms metode virke mer verbos, men sikrer sikkerhet. Ingen kjøretidsunntak for manglende felt eller typemismatch; hvis noe er galt, får du en kompileringsfeil.

`elm/xml`-dekoderfunksjonene er avhengig av å kartlegge XML-noder til Elm-typer. Du bygger dekodere som speiler dataens form, og sikrer at Elm-appen din håndterer XML like nøye som den gjør med sine egne interne datastrukturer.

Generering av XML er mindre vanlig i Elm, men kan oppnås med `elm/xml`s motstykke `Xml.Encode`.

## Se Også
- Elm Guide om JSON som også gjelder tankegangen rundt XML: [https://guide.elm-lang.org/interop/json.html](https://guide.elm-lang.org/interop/json.html)
- W3Cs XML-standard for en dypere forståelse av XML i seg selv: [https://www.w3.org/XML/](https://www.w3.org/XML/)
