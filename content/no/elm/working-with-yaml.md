---
title:                "Elm: Arbeide med yaml"
simple_title:         "Arbeide med yaml"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

# Hvorfor

Om du er en programmerer eller bare interessert i programmering, er det stor sjanse for at du har hørt om YAML. YAML, som står for "YAML Ain't Markup Language", er et konfigurasjonsformat som har blitt veldig populært de siste årene. Det er en enkel og intuitiv måte å strukturere data på, og det er spesielt nyttig for å konfigurere programmer og nettsider.

# Hvordan

For å jobbe med YAML i Elm, trenger vi å bruke et bibliotek som heter elm-yaml. Dette biblioteket lar oss lese og skrive YAML-filer ved hjelp av Elm-kode. La oss se på et enkelt eksempel på hvordan vi kan lese en YAML-fil og få ut dataen ved hjelp av elm-yaml:

```
Elm ...

import Yaml
import Yaml.Decode as Decode

type alias Person =
    { name : String
    , age : Int
    }

decoder : Decode.Decoder Person
decoder =
    Decode.map2 Person
        (Decode.field "name" Decode.string)
        (Decode.field "age" Decode.int)

output : Result String Person
output =
    """
    name: John
    age: 25
    """
    |> Yaml.parse
    |> Result.map (Yaml.decode decoder)

case output of
    Ok person ->
        Debug.log "Person" person

    Err err ->
        Debug.log "Error" err

```

I dette eksempelet har vi definert en Person-type som har et navn og en alder. Vi oppretter deretter en dekoder som passer til YAML-strukturen vår og bruker den til å lese dataen fra en tekststreng. Til slutt bruker vi Debug.log for å skrive ut resultatet. Dette er bare et enkelt eksempel på hvordan man kan jobbe med YAML i Elm, men det finnes mange flere muligheter og funksjonaliteter i dette biblioteket.

# Deep Dive

Et av de beste aspektene ved å jobbe med YAML i Elm er at det gir deg en enkel og lesbar måte å konfigurere data på. YAML er mye mindre "bråkete" enn tradisjonelle konfigurasjonsfiler som JSON eller XML. Det er også veldig fleksibelt, noe som gjør det enkelt å legge til eller endre data uten å måtte endre hele strukturen. Elm-biblioteket elm-yaml håndterer også feil og feilmeldinger på en god måte, slik at du enkelt kan se hva som har gått galt i YAML-filen din.

Det er også verdt å nevne at YAML er et mye mer ekspressivt format enn tradisjonelle konfigurasjonsfiler. Du kan bruke kommentarer, flerlinjede tekstblokker og andre funksjoner for å gjøre dataene dine mer leselige. Dette gjør det enklere å samarbeide med andre programmerere og å forstå komplekse datastrukturer.

# Se også

- Offisiell dokumentasjon for elm-yaml: https://package.elm-lang.org/packages/NoRedInk/elm-yaml/latest/
- En tutorial på hvordan bruke YAML i Elm: https://dev.to/paulweichhart/using-yaml-in-elm-13g1
- Elm-samfunnets forum om YAML: https://discourse.elm-lang.org/t/yaml-parsing-the-elm-way/1743