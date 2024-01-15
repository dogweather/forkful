---
title:                "Å jobbe med json"
html_title:           "C++: Å jobbe med json"
simple_title:         "Å jobbe med json"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor 
Fordi JSON er en av de mest populære formater for å overføre data mellom datamaskiner, og fordi det er enklere å håndtere enn andre formater som XML.

## Hvordan
For å bruke JSON med C++, må du ha en JSON-bibliotek installert. Det finnes mange alternativer, men en av de mest populære er nlohmann::json. Her er et eksempel på hvordan du kan lese og skrive JSON-data ved hjelp av dette biblioteket:

```C++
#include <nlohmann/json.hpp>
using json = nlohmann::json;

// Opprett et tomt JSON-objekt
json data;

// Legg til data
data["name"] = "Ola Nordmann";
data["age"] = 30;
data["married"] = true;

// Skriv ut som en string
std::cout << data.dump() << std::endl;

// Les fra en string
json parsedData = json::parse(data.dump());

// Hent ut verdier fra JSON-objektet
std::string name = parsedData["name"];
int age = parsedData["age"];
bool married = parsedData["married"];

// Skriv ut dataene
std::cout << "Navn: " << name << std::endl;
std::cout << "Alder: " << age << std::endl;
std::cout << "Gift: " << (married ? "Ja" : "Nei") << std::endl;
```

Dette vil gi følgende utskrift:

``` json
{"name": "Ola Nordmann", "age": 30, "married": true}
Navn: Ola Nordmann
Alder: 30
Gift: Ja
```

## Deep Dive
JSON står for "JavaScript Object Notation" og ble opprinnelig brukt til å strukturere data i JavaScript-applikasjoner. Det er et enkelt og lett format, som gjør det populært for å overføre data over nettverk. JSON består av to strukturer: en samling av nøkler og verdier, og en liste med verdier.

Det finnes ulike måter å lese og skrive JSON-data på i C++, men nlohmann::json gjør det enkelt ved å tillate tilgang til dataene som om det var et C++-objekt. Du kan også bruke std::map eller std::unordered_map for å lagre og håndtere JSON-data.

## Se også 
- [nlohmann::json](https://github.com/nlohmann/json)
- [JSON på w3schools](https://www.w3schools.com/js/js_json_intro.asp)
- [C++ på Codecademy](https://www.codecademy.com/learn/learn-c-plus-plus)