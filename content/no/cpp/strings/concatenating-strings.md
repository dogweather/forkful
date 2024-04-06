---
date: 2024-01-20 17:34:24.608629-07:00
description: "Slik gj\xF8r du: Historisk sett var strengsammensl\xE5ing en operasjon\
  \ som krevde h\xE5ndtering av char-arrays og null-terminert strenger i C. Med komme\
  \ av C++ og\u2026"
lastmod: '2024-04-05T21:53:42.053835-06:00'
model: gpt-4-1106-preview
summary: "Historisk sett var strengsammensl\xE5ing en operasjon som krevde h\xE5ndtering\
  \ av char-arrays og null-terminert strenger i C."
title: "Sammensl\xE5ing av strenger"
weight: 3
---

## Slik gjør du:
```C++
#include <iostream>
#include <string>

int main() {
    std::string hilsen = "Hei ";
    std::string navn = "Verden";
    std::string helHilsen = hilsen + navn + "!";
    
    std::cout << helHilsen << std::endl; // Skriver ut: Hei Verden!
    
    return 0;
}
```
```C++
#include <sstream>

int main() {
    std::ostringstream ss;
    ss << "Hei " << "Verden" << "!";
    
    std::cout << ss.str() << std::endl; // Skriver ut: Hei Verden!
    
    return 0;
}
```

## Dypdykk
Historisk sett var strengsammenslåing en operasjon som krevde håndtering av char-arrays og null-terminert strenger i C. Med komme av C++ og std::string-klassen, ble operasjonen mye sikrere og enklere. Et alternativ til `+` operatøren er `std::stringstream` som gir fin kontroll og er nyttig når man sammenslår mange forskjellige datatyper. Inne i C++ standardbiblioteket, slår `operator+` sammen strenger ved å kopiere innholdet, mens `std::string::append` eller `+=` operatøren legger til uten midlertidig kopiering, som kan være mer effektivt for store tekster.

## Se Også
- C++ Standard Library dokumentasjon om std::string: https://en.cppreference.com/w/cpp/string/basic_string
- C++ Standard Library dokumentasjon om stringstream: https://en.cppreference.com/w/cpp/io/basic_stringstream
- En guide til moderne C++ strenger: https://www.modernescpp.com/index.php/c-17-std-string-view
