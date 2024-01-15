---
title:                "Att hitta längden av en sträng"
html_title:           "C++: Att hitta längden av en sträng"
simple_title:         "Att hitta längden av en sträng"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Varför

Att hitta längden på en sträng kan vara en användbar färdighet vid programmering av dynamiska applikationer, där användaren kan mata in variabla mängd information. Det gör det möjligt att kontrollera och hantera olika typer av inmatningar.

# Hur man gör

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    // Definiera en sträng
    string namn = "Lisa";
    
    // Hitta längden på strängen och skriv ut 
    cout << "Längden på strängen " << namn << " är " << namn.length() << endl;
    
    // Ändra strängen
    namn += " Johansson";
    
    // Hitta längden på den nya strängen och skriv ut
    cout << "Längden på den nya strängen " << namn << " är " << namn.length() << endl;

    return 0;
}
```

```text
Längden på strängen Lisa är 4
Längden på den nya strängen Lisa Johansson är 13
```

# Djupdykning

I C++ finns det en inbyggd funktion som heter `length()` som returnerar längden på en given sträng. Denna funktion kontrollerar varje tecken i strängen och räknar sedan antalet tecken. Det finns också en alternativ funktion, `size()`, som returnerar samma värde som `length()` och kan användas på samma sätt.

Det är viktigt att veta att längden på en sträng börjar med index 0, vilket betyder att den första bokstaven i strängen har index 0 och den sista bokstaven har index Längd-1. Detta är viktigt att komma ihåg när man arbetar med strängar för att undvika felaktiga resultat.

# Se även

- [C++ strängar](https://www.w3schools.com/cpp/cpp_strings.asp)
- [C++ Standard Library](http://www.cplusplus.com/reference/string/string/length/)
- [C++ string class](https://www.geeksforgeeks.org/c-string-class-and-its-applications/)