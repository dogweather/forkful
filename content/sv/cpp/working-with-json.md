---
title:                "Arbeta med json"
html_title:           "C++: Arbeta med json"
simple_title:         "Arbeta med json"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## Vilket och varför? 
JSON (JavaScript Object Notation) är ett lättviktigt och enkelt format för att strukturera och lagra data. Det är särskilt populärt inom webbutveckling eftersom det enkelt kan läsas och skrivas av både människor och maskiner.

Programmerare använder JSON för att överföra och lagra data på ett enhetligt sätt, vilket gör det lättare att hantera och integrera data mellan olika system och applikationer.

## Så här gör du:
```C++
#include <iostream>
#include <nlohmann/json.hpp> // Header-fil för att arbeta med JSON-data
using namespace std;
using json = nlohmann::json; // Använda nlohmann biblioteket för att arbeta med JSON

int main() {
   // Skapa ett JSON-objekt
   json myObj = {
      {"förnamn", "Anna"},
      {"efternamn", "Andersson"},
      {"ålder", 25},
      {"intressen", {"programmering", "matlagning", "resor"}}
   };

   // Utöka JSON-objektet med en array av favoritmat
   myObj["favoritmat"] = {"sushi", "pizza", "sallad"};

   // Skriv ut värdet på "förnamn"
   cout << "Förnamn: " << myObj["förnamn"] << endl;

   // Omvandla json-objektet till en sträng
   string output = myObj.dump();

   // Skriv ut strängen
   cout << "JSON-data: " << output << endl;

   return 0;
}
```

Output:
```
Förnamn: Anna
JSON-data: {"förnamn":"Anna","efternamn":"Andersson","ålder":25,"intressen":["programmering","matlagning","resor"],"favoritmat":["sushi","pizza","sallad"]}
```

## Djupdykning:
JSON skapades ursprungligen av Douglas Crockford 2001 och har sedan dess blivit ett populärt format för datautbyte inom webbutveckling. Innan JSON användes ofta XML för strukturerad data, men på grund av dess komplexitet och tunga syntax blev JSON ett mer attraktivt alternativ.

Det finns också andra format för datastrukturering, som till exempel YAML och BSON. Men JSON är fortfarande den mest använda inom webbapplikationer på grund av sin enkelhet och lätthet att integrera med olika programmeringsspråk.

Implementeringen av JSON i C++ är möjlig genom olika bibliotek, som nlohmann/json som användes i exemplet ovan. Det finns också andra alternativ som JSON for Modern C++ och RapidJSON.

## Se även:
- [nlohmann/json bibliotekets hemsida](https://github.com/nlohmann/json)
- [En introduktion till JSON](https://www.json.org/json-sv.html)