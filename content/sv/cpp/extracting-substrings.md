---
title:    "C++: Extrahera delsträngar"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Substring, eller delsträng, är en viktig del av programmering då det tillåter oss att extrahera en del av en befintlig sträng. Detta kan vara användbart när vi exempelvis behöver manipulera en sträng eller jämföra en del av den med en annan sträng.

## Så här gör du

För att extrahera en substring i C++, används funktionen `substr()`. Vi börjar genom att deklarera en sträng variabel och sedan anropa `substr()` funktionen med två parametrar: startindex och längden på den del av strängen vi vill extrahera. Exempelvis:

```C++
#include <iostream> 

using namespace std; 

int main() { 
   string str = "Hej världen";
   string substr = str.substr(4, 6); // extraherar "världen" (startar på index 4 och har en längd på 6)

   // Utmatningen blir "världen"
   cout << substr << endl; 

   return 0; 
}
```

Det är viktigt att notera att indexet börjar på 0 och att längden inte får överstiga längden på den ursprungliga strängen. Om den andra parametern inte anges kommer `substr()` funktionen automatiskt att extrahera resten av strängen från startindex.

## Djupdykning

Förutom startindex och längd, finns det ytterligare två parametrar som kan användas för att anpassa extraheringsprocessen: delimiter (avgränsare) och antal utföranden. Delimiter används för att definiera ett tecken som kommer att markera slutet på den extraherade delsträngen. Om den inte anges kommer funktionen att fortsätta extrahera tills slutet av den ursprungliga strängen. Antal utföranden används för att ange hur många gånger extraktionen ska utföras, vilket är användbart när strängen innehåller flera delar som vi inte vill extrahera.

```C++
#include <iostream> 

using namespace std; 

int main() { 
   string str = "1,2,3,4,5";
   string substr = str.substr(2, 5, ','); // extraherar "2,3,4,5" (startar på index 2 och har en längd på 5 med "," som avgränsare)

   // Utmatningen blir "2,3,4,5"
   cout << substr << endl; 

   return 0; 
}
```

## Se även

- [C++ String Class Reference](https://www.cplusplus.com/reference/string/string/)
- [C++ substr() Function Reference](https://www.cplusplus.com/reference/string/string/substr/)