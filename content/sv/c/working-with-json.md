---
title:                "C: Att arbeta med json"
simple_title:         "Att arbeta med json"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/working-with-json.md"
---

{{< edit_this_page >}}

## Varför

JSON (JavaScript Object Notation) är en vanlig dataformat som används för att utbyta information mellan olika system och applikationer. Att kunna arbeta med JSON är en viktig och nödvändig färdighet för många programmerare, särskilt inom webbutveckling och datavetenskap.

## Så här gör du

För att kunna läsa, manipulera och skapa JSON-data i C-programmeringsspråket, behöver vi först inkludera biblioteket "json-c" genom att lägga till följande rad i vår kod:

```C
#include <json-c/json.h>
```

Efter att vi har inkluderat biblioteket kan vi använda funktioner som tillhandahålls för att arbeta med JSON-data. Här är ett enkelt exempel på hur vi kan läsa in och skriva ut JSON-data:

```C
// Skapa ett JSON-objekt
json_object *my_obj = json_object_new_object();

// Lägg till nycklar och värden till objektet
json_object_object_add(my_obj, "name", json_object_new_string("Lisa"));
json_object_object_add(my_obj, "age", json_object_new_int(25));

// Konvertera objektet till en sträng
const char *my_json_str = json_object_to_json_string(my_obj);

// Skriv ut strängen
printf("JSON: %s\n", my_json_str);

// Avsluta
json_object_put(my_obj);
```

Detta kommer att producera följande utmatning:

```
JSON: {"name": "Lisa", "age": 25}
```

Förutom att läsa och skriva ut JSON-data, kan vi också bearbeta och manipulera data på olika sätt. Till exempel kan vi använda funktionen "json_object_put" för att ta bort ett objekt från en JSON-sträng eller ändra värden för en viss nyckel.

## Djupdykning

För dem som är mer erfarna och vill utforska mer komplexa funktioner, finns det en mängd olika alternativ och metoder för att arbeta med JSON-data i C. Biblioteket "json-c" erbjuder en mängd olika datatyper och funktioner för att manipulera JSON-data på olika sätt.

En annan intressant funktion som kan vara användbar är "json_object_to_file", som låter oss spara vårt JSON-objekt till en fil. På så sätt kan vi enkelt lagra och hämta data från andra system eller applikationer.

## Se även

- [json-c GitHub Repository](https://github.com/json-c/json-c)
- [C-programmeringsspråket](https://sv.wikipedia.org/wiki/C_(programspr%C3%A5k))
- [Vad är JSON?](https://www.json.org/json-en.html)