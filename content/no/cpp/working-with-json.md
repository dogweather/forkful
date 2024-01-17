---
title:                "Arbeid med json"
html_title:           "C++: Arbeid med json"
simple_title:         "Arbeid med json"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/working-with-json.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?

JSON står for JavaScript Object Notation, og er et lettvekts format for å lagre og utveksle datastrukturer. Det er spesielt populært blant webutviklere på grunn av dets enkle syntaks og støtte på tvers av ulike programmeringsspråk.

Hvordan:

Å jobbe med JSON i C++ er enkelt og effektivt. Først må vi inkludere "json.hpp" biblioteket i vår kode. Deretter kan vi bruke funksjoner som "parse()" og "serialize()" for å lese og skrive JSON-data fra en fil eller et nettverk. Et eksempel på hvordan dette kan se ut er:

```C++
#include <iostream>
#include "json.hpp"

using json = nlohmann::json; // Definerer et alias for "json.hpp" biblioteket.

int main() {
    // Oppretter et JSON-objekt og legger til noen verdier.
    json myObj = {
        {"navn", "Per"},
        {"alder", 25},
        {"favoritt_farger", {"blå", "grønn", "gul"}}
    };

    // Skriver ut JSON-dataen på konsollen.
    std::cout << myObj.dump() << std::endl;

    // Leser JSON-data fra en fil og skriver ut verdien til et bestemt element.
    json myFile = json::parse(std::ifstream("minfil.json"));
    std::cout << "Alderen til Per er: " << myFile["alder"] << std::endl;

    return 0;
}
```

Utfordringen med JSON i C++ er at det ikke finnes noen standardbiblioteker eller innebygde funksjoner for å jobbe med det. Men heldigvis finnes det flere populære tredjepartsbiblioteker som "json.hpp" og "rapidjson" som gjør det enklere å håndtere JSON-data.

Dypdykk:

JSON ble først utviklet av Douglas Crockford på 1990-tallet som en del av JavaScript-programmeringsspråket. I dag er det et utbredt format som brukes over hele verden, spesielt i webutvikling. Alternativer til JSON inkluderer XML og YAML, men disse har ofte mer komplekse syntakser og er derfor ikke like populære.

For å implementere JSON i C++, må man enten lage en parser og serializer for JSON-dataen selv, eller bruke et tredjepartsbibliotek. Som nevnt tidligere finnes det flere populære alternativer, og valget avhenger ofte av personlige preferanser og prosjektets behov.

Se også:

- "json.hpp" biblioteket: https://github.com/nlohmann/json
- "rapidjson" biblioteket: https://rapidjson.org/