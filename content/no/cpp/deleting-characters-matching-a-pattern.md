---
title:    "C++: Slette tegn som matcher et mønster"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor
Noen ganger kan det være nødvendig å slette bestemte tegn fra en tekststreng i et C++-program. Dette kan være for å fjerne uønskede tegn eller for å gjøre teksten mer lesbar og strukturert. Uansett hva grunnen måtte være, så kan det å slette tegn som matcher et mønster være en nyttig teknikk i programmering.

## Hvordan
For å slette tegn som matcher et mønster i C++, kan man bruke funksjonen `std::regex_replace`. Denne funksjonen tar inn tre parametere: tekststrengen man ønsker å endre, mønsteret som tegnene skal matches mot og hva de skal erstattes med. Her er et eksempel på hvordan man kan bruke denne funksjonen:

```C++
#include <iostream>
#include <string>
#include <regex>

int main() {
  std::string tekst = "Jeg liker å spise frukt, men ikke epler";
  std::regex mønster("frukt|epler");
  std::string resultat = std::regex_replace(tekst, mønster, "grønnsaker");
  std::cout << resultat;
  // Output: Jeg liker å spise grønnsaker, men ikke grønnsaker
  return 0;
}
```

I dette eksempelet vil både "frukt" og "epler" bli erstattet med "grønnsaker", selv om de opptrer på forskjellige steder i tekststrengen.

## Dypdykk
Det er mange forskjellige måter å bruke `std::regex_replace` på, avhengig av hvilke mønstre man ønsker å matche og hvordan man ønsker å erstatte dem. Det er også mulig å bruke såkalte "regular expressions" i mønsteret, som gjør det mulig å matche mer komplekse mønstre.

Det finnes også andre funksjoner i C++ som kan brukes til å slette tegn som matcher et mønster, som for eksempel `std::replace` og `std::remove_if`. Disse funksjonene kan være nyttige hvis man ønsker å slette et enkelt tegn eller enkelttegn på en bestemt plassering i tekststrengen.

## Se Også
- [Cppreference - std::regex_replace](https://en.cppreference.com/w/cpp/regex/regex_replace)
- [GeeksforGeeks - std::regex_replace](https://www.geeksforgeeks.org/stdregex_replace-in-cpp/)