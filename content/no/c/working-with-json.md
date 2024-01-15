---
title:                "Å jobbe med json"
html_title:           "C: Å jobbe med json"
simple_title:         "Å jobbe med json"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor ville du jobbe med JSON i C? Det er enkelt: JSON er den vanligste formen for datautveksling på nettet, og ved å lære å jobbe med det i C kan du gjøre programmene dine mer fleksible og robuste.

## Hvordan

La oss se på et eksempel på hvordan du kan jobbe med JSON i C. Først må vi inkludere "json-c/json.h" biblioteket og definere en variabel for å holde JSON-dataen:

```C
#include <json-c/json.h>
json_object *jobj;
```

Vi kan bruke en annen funksjon fra biblioteket for å lage en ny JSON-objekt:

```C
jobj = json_object_new_object();
```

For å legge til data i JSON-objektet vårt, kan vi bruke følgende kode:

```C
json_object_object_add(jobj, "name", json_object_new_string("Sara"));
json_object_object_add(jobj, "age", json_object_new_int(25));
```

Til slutt kan vi bruke en funksjon for å konvertere JSON-objektet til en streng og skrive den ut:

```C
printf("%s\n", json_object_to_json_string(jobj));
```

Koden vår vil produsere følgende utdata:

```C
{"name": "Sara", "age": 25}
```

Vær oppmerksom på at dette bare er et enkelt eksempel på hvordan du kan jobbe med JSON i C. Det finnes mange flere funksjoner og metoder som kan hjelpe deg med å lese og manipulere JSON-data.

## Dypdykk

For å jobbe mer effektivt med JSON i C, kan det være nyttig å forstå noen grunnleggende konsepter. JSON består av nøkler og verdier, der nøklene er strenger og verdiene kan være av forskjellige typer (strenger, tall, objekter, osv.). Det er viktig å forstå denne strukturen når du leser og skriver JSON-data.

En annen ting å vurdere er hvordan du håndterer feil når du jobber med JSON i C. Biblioteket har funksjoner for å sjekke om et gitt nøkkel eller verdi eksisterer, og for å håndtere eventuelle feil som måtte oppstå under lesing eller skriving av JSON-data.

Et godt sted å starte for å lære mer om å jobbe med JSON i C er selve dokumentasjonen til "json-c" biblioteket, som har en liste over alle tilgjengelige funksjoner og hvordan de kan brukes.

## Se også

- [json-c dokumentasjon](https://linux.die.net/man/3/json-c)
- [JSON offisiell nettside](https://www.json.org/json-en.html)
- [C-programmering kurs (på norsk)](https://www.ntnu.no/wiki/display/ieg/5.+C-Programmering)