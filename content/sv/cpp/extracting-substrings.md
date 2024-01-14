---
title:                "C++: Extrahering av substrängar"
simple_title:         "Extrahering av substrängar"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Att extrahera substrängar är en vanlig uppgift för många programmerare, särskilt inom C++. Det är en användbar teknik för att manipulera strängar på ett effektivt sätt och kan hjälpa till att lösa många problem inom programutveckling.

## Hur man gör

För att extrahera en substring från en sträng i C++ kan du använda den inbyggda funktionen `substr()`. Detta används för att få ut en del av en sträng med hjälp av index och längd. Här är ett exempel på hur du kan extrahera en del av en sträng med hjälp av `substr()`:

```C++
#include <iostream>
using namespace std;

int main()
{
    string str = "Hej, jag heter Anna!";
    
    // Extrahera "Anna" från strängen
    string substring = str.substr(16, 4);

    cout << "Substring: " << substring << endl;
    
    return 0;
}
```

Output: "Substring: Anna"

## Djupdykning

När du extraherar substrängar är det viktigt att förstå hur index och längd fungerar. Indexet i `substr()` är den position där delsträngen börjar, medan längden är antalet tecken som ska extraheras från det positionen. Det är också viktigt att komma ihåg att den första positionen i en sträng är index 0.

Det finns också flera andra sätt att extrahera substrängar i C++, som att använda `find()` och `substr()` tillsammans eller använda `string::npos` för att extrahera en del av en sträng baserat på ett visst villkor.

## Se även

- [C++ Reference - substr()](https://www.cplusplus.com/reference/string/string/substr/)
- [GeeksforGeeks - Different methods of extracting substrings in C++](https://www.geeksforgeeks.org/string-append-c/)
- [C++ String Functions](https://www.programiz.com/cpp-programming/library-function/string)