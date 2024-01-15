---
title:                "Arbeide med YAML"
html_title:           "Elm: Arbeide med YAML"
simple_title:         "Arbeide med YAML"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du jobber med frontend-utvikling, har du kanskje hørt om YAML, men lurer på hva det egentlig er. YAML står for "YAML Ain't Markup Language" og det er et verktøy som kan hjelpe deg med å strukturere og organisere data på en enklere måte. Hvis du ønsker å forbedre utviklingsprosessen din og lage mer lesbar kode, kan læring av YAML være en god idé.

## Hvordan

For å begynne å bruke YAML i Elm, må du først legge til en avhengighet i prosjektet ditt:

```Elm
import Yaml exposing (scalar, list, mapping, decode)
```

Deretter kan du begynne å definere dine YAML-data. Her er et eksempel på en enkel liste med ulike språk:

```Elm
languages : Yaml.Value
languages =
    list
        [ scalar "Javascript"
        , scalar "Elm"
        , scalar "Python"
        , scalar "PHP"
        ]
```

For å konvertere YAML-dataene til Elm, kan du bruke funksjonen `decode`:

```Elm
elmLanguages : Result String (List String)
elmLanguages =
    case decode languages of
        Ok result ->
            result

        Err error ->
            Err (Debug.toString error)
```

Etter å ha kjørt koden ovenfor, vil du få en liste med språk som kan brukes i ditt Elm-prosjekt.

## Dypdykk

YAML har flere interessante funksjoner som kan gjøre utviklingsprosessen din mer effektiv. For eksempel kan du bruke flere typer datastrukturer som `scalar`, `list`og `mapping` for å bygge mer komplekse YAML-data. Du kan også legge til kommentarer i dine YAML-filer for å gjøre det enklere å forstå koden senere.

Føler du deg klar til å begynne å bruke YAML i ditt Elm-prosjekt? Start med å utforske dokumentasjonen for mer informasjon og flere muligheter.

## Se Også

- [YAML offisiell hjemmeside](https://yaml.org/)
- [Elm dokumentasjon om YAML](https://package.elm-lang.org/packages/mdgriffith/yaml/latest/)