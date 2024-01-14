---
title:                "C++: Arbeid med json"
simple_title:         "Arbeid med json"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor

Innlemming og bruk av JSON-filer er en viktig del av moderne programmering. JSON (JavaScript Object Notation) er et populært format for lagring og utveksling av data. Ved å lære å jobbe med JSON, kan du gjøre programmene dine mer robuste og fleksible, og dermed sparer du tid og krefter i utviklingsprosessen.

## Hvordan

For å jobbe med JSON i C++, trenger du en parser, som kan tolke og lese JSON-data. I dette eksempelet bruker vi den populære Biblioteket "RapidJSON".

```C++
#include <iostream>
#include "rapidjson/document.h"

int main() {
    // Opprette en JSON-streng
    const char* json = "{ \"navn\": \"John\", \"alder\": 30, \"by\": \"Oslo\"}";

    // Konvertere strengen til en RapidJSON dokument
    rapidjson::Document doc;
    doc.Parse(json);

    // Hente og skrive ut dataene fra dokumentet
    const char* navn = doc["navn"].GetString();
    int alder = doc["alder"].GetInt();
    const char* by = doc["by"].GetString();

    std::cout << "Navn: " << navn << std::endl;
    std::cout << "Alder: " << alder << std::endl;
    std::cout << "By: " << by << std::endl;

    return 0;
}
```

Output fra koden vil være:

```
Navn: John
Alder: 30
By: Oslo
```

Du kan også jobbe med mer komplekse JSON-strukturer, som for eksempel å hente ut verdier fra en liste:

```C++
// Opprette en JSON-streng
const char* json = "{ \"navn\": \"Lisa\", \"interesser\": [\"Programmering\", \"Musikk\", \"Sport\"] }";
    
// Konvertere strengen til en RapidJSON dokument
rapidjson::Document doc;
doc.Parse(json);

// Hente og skrive ut interesse-listen
const char* navn = doc["navn"].GetString();

std::cout << "Navn: " << navn << std::endl;
std::cout << "Interesser: " << std::endl;

// Looper gjennom interesser-listen og skriver ut hver verdi
for (auto& interesse : doc["interesser"].GetArray()) {
    std::cout << "- " << interesse.GetString() << std::endl;
}
```

Output fra koden vil være:

```
Navn: Lisa
Interesser:
- Programmering
- Musikk
- Sport
```

## Dypdykk

RapidJSON tilbyr en rekke funksjoner for å jobbe med JSON-data, inkludert manipulering, validering og løkketraversering. Det er også viktig å håndtere feil og håndtere ugyldige JSON-data for å sikre pålitelighet og robusthet i programmene dine.

For å lære mer om RapidJSON og andre nyttige verktøy for å jobbe med JSON i C++, kan du utforske disse ressursene:

- [RapidJSON dokumentasjon](https://rapidjson.org/)
- [JSON tutorials for C++](https://www.cplusplus.com/doc/tutorial/files/)
- [C++ JSON biblioteker sammenligning](https://developerinsider.co/best-cpp-json-library/)
- [Stack Overflow forum for JSON-relaterte spørsmål](https://stackoverflow.com/questions/tagged/json+c%2b%2b)

## Se også

- [Introduksjon til JSON (Norwegian)](https://medium.com/swlh/learn-to-code-with-json-part-1-introduction-to-json-109652cda3b3)
- [Bygge og håndtere JSON-objekter i C++ (Norwegian)](https://www.programering.com/q/MDO5MzNwATA.html)
- [Parsing JSON med RapidJSON i C++ (Norwegian)](https://danimoth.com/json-parser/)