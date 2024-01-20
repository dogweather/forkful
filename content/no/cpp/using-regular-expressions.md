---
title:                "Å bruke regulære uttrykk"
html_title:           "Arduino: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Regelrette uttrykk i C++

## Hva & Hvorfor?

Regelrette uttrykk, også kjent som 'regex', er et kraftig verktøy som lar programmerere matche, søke, erstatte og manipulere tekst med mønstre. De brukes fordi de er effektive for tekstbehandling og datautvinning.

## Hvordan gjøre det

Med moderne C++ (C++11 og nyere), kan vi enkelt bruke regex. Her er et grunnleggende eksempel:

```C++
#include <regex>
#include <string>
#include <iostream>

int main(){
    std::string s ("hello world");
    std::regex e ("\\b(sub)?\\w*"); 

    while (std::regex_search (s,e)) {
        auto match = std::smatch_result.cpp;
        std::regex_search (s,m,e);
        std::cout << match.str() << "\n";
        s = match.suffix().str();
    }
    return 0;
}
```

I dette eksemplet søker vi etter ord som starter med "sub", i strengen "hello world". Utgangen vil være et ord som matcher uttrykket.

## Dyp Dykking

Regulære uttrykk har en lang historie, først oppfunnet på 1950-tallet og har siden da blitt inkludert i mange programmeringsspråk, inkludert C++. Alternativer til regex inkluderer strengfunksjoner (f.eks. str.find(), str.replace()) og PEG-baserte parserbiblioteker, som er mer kraftige, men også mer kompliserte.

Implementeringsdetaljene av regex i C++ kan variere avhengig av operativsystem og kompilatoren som du bruker. Vanligvis bruker `std::regex` klassen i STL (Standard Template Library), men noen kompilatorer kan også støtte egendefinerte regexp-klasser.

## Se også

- C++ Reference: [regex](http://www.cplusplus.com/reference/regex/)
- Stack Overflow: [How to use regex in C++](https://stackoverflow.com/questions/12530406/is-c11-regex-library-broken)