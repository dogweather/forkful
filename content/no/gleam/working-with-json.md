---
title:                "Gleam: Å jobbe med json"
simple_title:         "Å jobbe med json"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor

JSON er en utrolig viktig del av moderne webutvikling, og å kunne arbeide med JSON er en nøkkelkompetanse for enhver programmerer. Å forstå hvordan data er strukturert i JSON-format og kunne behandle det riktig er avgjørende for å bygge effektive og funksjonelle nettsider og applikasjoner.

## Hvordan

For å håndtere JSON-data i Gleam, kan du bruke det innebygde `Json.Decode` biblioteket. Det gir funksjoner for å mappe JSON-data til dine egne typer og gjøre de om til Gleam-records. La oss se på et eksempel:

```Gleam
let data =
  """
  {
    "name": "Maria",
    "age": 28,
    "hobbies": ["skiing", "hiking"]
  }
  """
let result = Json.Decode.object(data)
```

I dette tilfellet blir `result` en `Ok` med en Gleam-record som ser slik ut:

```Gleam
{
  name: "Maria",
  age: 28,
  hobbies: ["skiing", "hiking"]
}
```

Vi kan også bruke funksjonen `Json.Decode.map` for å konvertere dataen til vår egen record-type. Anta at vi har følgende record-type definert:

```Gleam
pub struct Person(name: String, age: Int, hobbies: List(String))
```

Vi kan bruke `Json.Decode.map` for å kartlegge JSON-data til vår `Person`-type:

```Gleam
let result = Json.Decode.map(Person, data)
```

Nå vil `result` være en `Ok` med en `Person`-record som inneholder navnet, alderen og en liste over interesser.

## Deep Dive

En annen nyttig funksjon i `Json.Decode` biblioteket er `Json.Decode.field`, som lar deg ekstrahere en spesifikk verdi fra JSON-objekter eller matriser. Du kan også bruke `Json.Decode.at` for å få tak i dataen på dypere nivåer i JSON-strukturen.

Videre gir `Json.Decode.one_of` funksjonen deg muligheten til å håndtere forskjellige typer data som kan komme fra forskjellige kilder. For eksempel kan du bruke `Json.Decode.one_of` for å håndtere både JSON-objekter og lister av JSON-objekter.

## Se også

- [Gleam Docs - JSON-dekoding](https://gleam.run/documentation/stdlib/json_decode/)
- [JSON.org - The JSON Data Interchange Format](https://www.json.org/json-en.html)