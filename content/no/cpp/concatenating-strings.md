---
title:                "Sammenslåing av strenger"
date:                  2024-01-20T17:34:24.608629-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sammenslåing av strenger"

category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Strengsammenslåing handler om å sette sammen to eller flere tekststykker for å danne én enhet. Programmerere gjør det for å bygge opp meldinger, lage dynamisk innhold eller strukturert data.

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
