---
title:                "Att arbeta med json"
html_title:           "C++: Att arbeta med json"
simple_title:         "Att arbeta med json"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/working-with-json.md"
---

{{< edit_this_page >}}

# Varför

JSON är enligt många utvecklare ett måste i deras toolkit. Det är ett populärt format för datautbyte på webben och används ofta i APIer och databaser. Genom att lära dig hur man arbetar med JSON, kommer du att kunna hantera och manipulera data på ett effektivt sätt.

# Så här gör du

Det första steget för att arbeta med JSON i C++ är att inkludera biblioteket "nlohmann/json" i ditt projekt. Sedan kan du enkelt skapa, tolka och manipulera JSON-objekt med hjälp av kodexempel nedan.

```C++
// Skapa ett tomt JSON-objekt
nlohmann::json min_json;

// Lägg till en nyckel och ett värde
min_json["namn"] = "Lisa";
min_json["ålder"] = 25;
```

För att få en strukturerad utskrift av ditt JSON-objekt kan du använda funktionen `dump()` som finns tillgänglig i biblioteket. Detta kommer att skriva ut hela ditt JSON-objekt med nycklar och värden.

```C++
// Skriv ut ditt JSON-objekt
std::cout << min_json.dump() << std::endl;

// Output:
// {
//    "namn": "Lisa",
//    "ålder": 25
// }
```

För att läsa och tolka en JSON-fil i ditt projekt kan du använda funktionen `parse()` som finns i biblioteket. Detta kommer att översätta en JSON-fil till ett JSON-objekt som du kan använda som du vill.

```C++
// Läs och tolka en JSON-fil
nlohmann::json ny_json = nlohmann::json::parse("mitt_json.json");

// Hämta ett värde från JSON-objektet
std::string namn = ny_json["namn"];
std::cout << "Välkommen " << namn << ", till din JSON-fil!" << std::endl;

// Output:
// Välkommen Lisa, till din JSON-fil!
```

# Djupdykning

Ett annat sätt att lägga till nycklar och värden i ditt JSON-objekt är att använda funktionen `emplace()`. Detta gör att du kan lägga till en nyckel och ett värde i ett JSON-objekt utan att behöva använda `[]` operatorn.

```C++
// Lägg till en nyckel och ett värde med emplace()
ny_json.emplace("jobb", "webbutvecklare");
```

För att komma åt specifika delar av ditt JSON-objekt kan du använda `find()` funktionen. Detta returnerar en iterator som pekar på nyckeln och värdet som du söker.

```C++
// Hitta specifikt värde med find()
auto it = ny_json.find("jobb");

// Skriv ut resultatet
if (it != ny_json.end()) {
    std::cout << "Ditt jobb: " << *it << std::endl;
}

// Output:
// Ditt jobb: webbutvecklare
```

Genom att läsa på om nlohmann/json biblioteket och experimentera med olika funktioner och metoder, kan du utveckla dina kunskaper om hur man arbetar med JSON i C++.

# Se även

* Officiell dokumentation för nlohmann/json: https://github.com/nlohmann/json
* En tutorial om hur man använder JSON i C++: https://www.programiz.com/cpp-programming/library/json