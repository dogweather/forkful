---
title:                "Extrahera delsträngar"
html_title:           "C++: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför 
Att extrahera substrängar från en större sträng kan vara en användbar teknik för att manipulera data på ett effektivt sätt. Till exempel kan det vara till hjälp för att isolera vissa delar av en textsträng eller för att söka efter specifika mönster inom en sträng.

## Så här gör du 
För att extrahera substrängar i C++ kan du använda funktionen `substr()` som finns inbyggd i `string` biblioteket. Funktionen tar två parametrar, en startposition och en längd, och returnerar en ny sträng som är en del av den ursprungliga strängen. Här är ett exempel på hur man kan använda funktionen för att extrahera en del av en sträng: 

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
    string str = "Hello World";
    string sub = str.substr(6,5); // extrahera från position 6, 5 tecken
    cout << sub << endl; // Skriver ut "World" 
    return 0;
}
```

För att extrahera en del av en sträng baserat på ett specifikt mönster kan du använda `find()` funktionen för att hitta positionen för det mönstret och sedan använda `substr()` för att extrahera en del av strängen baserat på den positionen. Här är ett exempel på hur man kan göra det: 

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
    string str = "Det var en gång en liten groda";
    int pos = str.find("gång"); // Hitta positionen av "gång"
    string sub = str.substr(pos, 8); // Extrahera "gång en"
    cout << sub << endl; // Skriver ut "gång en"
    return 0;
}
```

## Djupdykning 
När man använder `substr()` funktionen är det viktigt att tänka på att den första positionen är 0, inte 1. Det betyder att om du vill extrahera de första fem tecknen av en sträng, så är startpositionen 0 och längden 5. Dessutom kan man också lägga till en tredje parameter till `substr()` funktionen för att bara extrahera en del av en sträng, till exempel bara de första tre tecknen.

## Se även 
- [C++ string documentation](https://www.cplusplus.com/reference/string/string/)
- [Using substr() in C++ to manipulate strings](https://www.geeksforgeeks.org/using-substr-c-manipulate-strings/)