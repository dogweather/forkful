---
date: 2024-01-20 17:34:21.430573-07:00
description: "How to: Konkatenering med `+` operat\xF6ren."
lastmod: '2024-03-13T22:44:38.201120-06:00'
model: gpt-4-1106-preview
summary: "Konkatenering med `+` operat\xF6ren."
title: "Sammanslagning av str\xE4ngar"
weight: 3
---

## How to:
Konkatenering med `+` operatören:
```C++
#include <iostream>
#include <string>

int main() {
    std::string firstName = "Karl";
    std::string lastName = "Svensson";
    std::string fullName = firstName + " " + lastName; // Lägger till ett mellanslag mellan förnamn och efternamn

    std::cout << "Hela namnet: " << fullName << std::endl; // Skriver ut "Hela namnet: Karl Svensson"
    return 0;
}
```
Använda `append()` funktionen:
```C++
#include <iostream>
#include <string>

int main() {
    std::string city = "Stockholm";
    std::string greeting = "Välkommen till ";
    greeting.append(city); // Lägger till 'city' till 'greeting'

    std::cout << greeting << std::endl; // Skriver ut "Välkommen till Stockholm"
    return 0;
}
```

## Deep Dive:
Förr i tiden var C++ strängkonkatenering begränsad till C-stilsträngar och krävde manuellt arbete med teckentabeller. Nu används `std::string` klassen, en del av standardbiblioteket, för enklare och säkrare hantering.

Alternativ till `+` och `append()` inkluderar `stringstream` och `fmt` biblioteket (i modern C++), som båda hanterar mer komplexa string manipuleringar.

Implementationen av konkatenering är effektiviserad i moderna kompilatorer, men överdriven användning kan fortfarande leda till prestandaförluster, särskilt i stora loopar eller när man hanterar stora textmängder.

## See Also:
- C++ Reference for std::string: https://cplusplus.com/reference/string/string/
- C++ Standard Library documentation: https://en.cppreference.com/w/cpp/header/string
- Stringstream documentation: https://en.cppreference.com/w/cpp/io/basic_stringstream
- Fmt library (for modern C++): https://github.com/fmtlib/fmt
