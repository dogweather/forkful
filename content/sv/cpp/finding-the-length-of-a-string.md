---
title:                "C++: Hitta längden på en sträng"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Varför

När du skriver kod i C++ är det viktigt att ha koll på olika datastrukturer och funktioner som kan underlätta din programmering. En sådan funktion är "length", som används för att hitta längden på en sträng. Att kunna hitta längden på en sträng kan vara användbart när du till exempel behöver kontrollera om en inmatning från användaren är inom ett visst gränsvärde eller när du behöver manipulera strängen på något sätt.

## Så här gör du

Att hitta längden på en sträng i C++ är enkelt. Du kan använda funktionen "length" som är en del av standardbiblioteket <string>. Funktionen tar inga argument och returnerar en "int" som motsvarar längden på strängen. Här är ett exempel på hur du kan använda funktionen i din kod:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string namn = "Karin";

    // Här använder vi funktionen "length" för att hitta längden på strängen "Karin"
    int langd = namn.length();

    cout << "Längden på strängen Karin är: " << langd << endl;

    return 0;
}
```

Output:

```
Längden på strängen Karin är: 5
```

## Djupdykning

Funktionen "length" är en metoden för klassen "string". En klass är en mall för objekt som definierar dess beteende och egenskaper. För att använda en klass som "string" måste du inkludera dess tillhörande standardbibliotek. Det finns också en liknande funktion som heter "size", som också används för att hitta längden på en sträng men som returnerar en "size_t" istället för en "int". "size_t" är en datatyp som är integer av högsta möjliga storlek i systemet och är därför mer lämplig för större strängar.

# Se även

- [C++ standardbibliotek](https://www.cplusplus.com/reference/string/)
- [C++ Klasser och objekt](https://www.cplusplus.com/doc/tutorial/classes/)
- [Mer om datastrukturer i C++](https://www.geeksforgeeks.org/data-structures-in-cpp/)