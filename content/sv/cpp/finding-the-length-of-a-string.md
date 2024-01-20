---
title:                "Hitta längden på en sträng"
html_title:           "Arduino: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att hitta längden på en sträng betyder att avgöra antalet tecken i den. Programmerare gör detta för olika ändamål; till exempel för att räkna ord, kontrollera input-validiteten, och manipulera data. 

## Hur Kan Man:

Här är hur du hittar längden på en sträng i C++.

```C++
#include <iostream>
#include <string>
    
int main() {
    std::string str = "Hej världen!";
    std::cout << "Strängens längd är: " << str.length() << '\n';
    return 0;
}
```

Med denna kod skickas följande utdata till konsolen:

```
Strängens längd är: 12
```

Funktionen `length()` används för att räkna antalet tecken i strängen "Hej världen!".

## Djupdykning

Historiskt sett, i tidigare versioner av C, användes `strlen()`-funktionen i stället för `length()`. Den `strlen()`-funktionen räknar tecknen till det hittar ett nulltecken (`/0`), vilket markerar slutet av strängen. I C++ är `length()` och `size()` funktioner tillgängliga och rekommenderade.

Alternative sätt att hitta längden på en sträng inkluderar att använda `std::distance` med `std::begin` och `std::end` som parametrar, eller ett `for`- eller `while`-loop.

När det gäller implementeringsdetaljer, det är värt att notera att både `length()` och `size()` returnerar antalet tecken i strängen exklusive nulltecknet.

## Se Även

För ytterligare läsning, hänvisas läsaren till följande källor:

2. [C++ string::length - C++ 98, C++ 11 - cppreference.com](https://en.cppreference.com/w/cpp/string/basic_string/length)
3. [The C++ String Toolkit Library (StrTk) Split function - CodeProject](https://www.codeproject.com/Articles/21086/The-C-String-Toolkit-Library-StrTk-Split-function)