---
title:                "Interpolering av streng"
html_title:           "C++: Interpolering av streng"
simple_title:         "Interpolering av streng"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Hva og hvorfor?
Interpolering av strenger er en prosess der en variabel eller uttrykk blir satt inn i en streng på et bestemt sted. Dette er en vanlig teknikk brukt av programmører for å lage dynamiske og tilpassede tekststrenger i sine programmer.

# Hvordan:
```C++
#include <iostream>
#include <string>

int main() {
  std::string navn = "Ola";
  std::string alder = "25";
  std::string uttrykk = "Hei, mitt navn er " + navn + " og jeg er " + alder + " år gammel.";

  std::cout << uttrykk << std::endl;
  
  return 0;
}

/* Utskrift:
Hei, mitt navn er Ola og jeg er 25 år gammel.
*/
```

# Dypdykk:
Interpolering av strenger har vært en del av programmering siden tidlig på 1960-tallet. Det finnes også andre måter å lage dynamiske strenger på, som for eksempel formatering med `sprintf`-funksjonen.

Implementasjonen av interpolering av strenger kan variere mellom ulike programmeringsspråk, men prinsippet er alltid det samme - å erstatte variabler eller uttrykk med deres faktiske verdier i en streng.

# Se også:
- [C++ string documentation](https://www.cplusplus.com/reference/string/)
- [Hvordan bruke std::format i C++20](https://www.modernescpp.com/index.php/std-format-in-c-20)