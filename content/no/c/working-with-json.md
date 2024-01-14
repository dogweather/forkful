---
title:                "C: Arbeid med json"
simple_title:         "Arbeid med json"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor

JSON (JavaScript Object Notation) er et populært format for å utveksle og lagre data i programmering. Det er en enkel og lettvektsmåte å representere data på, som gjør det enkelt å lese og tolke. Ved å lære å jobbe med JSON, vil du kunne utvide din programmeringsferdigheter og kunne samarbeide med andre programmerere på en effektiv måte.

## Hvordan

For å jobbe med JSON i C, må du inkludere headerfilen `json-c/json.h` i koden din. Deretter kan du bruke funksjoner som `json_object_new_object()` for å lage et tomt JSON-objekt, og `json_object_object_add()` for å legge til nøkler og verdier i objektet. La oss se på et eksempel:

```C
#include <stdio.h>
#include <json-c/json.h>

int main()
{
    // Oppretter et nytt JSON-objekt
    struct json_object *data = json_object_new_object();

    // Legger til en nøkkel og verdi i objektet
    json_object_object_add(data, "navn", json_object_new_string("Ole"));

    // Skriver ut JSON-objektet som en streng
    printf("%s\n", json_object_to_json_string(data));
    
    return 0;
}
```

Dette vil resultere i følgende output:

```
{"name":"Ole"}
```

Du kan også jobbe med mer komplekse strukturer, som lister og innlekte objekter, ved å bruke passende funksjoner som `json_object_new_array()` og `json_object_get_object()`.

## Dypdykk

Et dykker en dypere forståelse av JSON i C, må man også kunne jobbe med parsing og serialisering av data. JSON-C inneholder funksjoner som `json_object_to_file()` for å skrive et JSON-objekt til en fil, og `json_object_from_file()` for å lese et JSON-objekt fra en fil. Disse funksjonene er nyttige for å lagre data og hente det senere.

I tillegg tilbyr JSON-C også muligheten til å validere JSON-data for å sikre at de følger riktig syntaks og struktur. Dette kan gjøres ved å bruke funksjonen `json_tokener_parse_ex()`. Hvis JSON-dataen ikke er gyldig, vil denne funksjonen returnere en feilkode og en beskrivelse av feilen.

## Se Også

- [JSON-C dokumentasjon](https://json-c.github.io/json-c/)
- [Introduksjon til JSON i C](https://www.drdobbs.com/cpp/json-basics-in-c/240165293)
- [Arbeide med JSON-data i C](https://programming.vip/docs/c-json-tutorial.html)