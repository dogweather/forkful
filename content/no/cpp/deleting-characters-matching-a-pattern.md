---
title:                "Sletting av tegn som matcher et mønster"
html_title:           "C++: Sletting av tegn som matcher et mønster"
simple_title:         "Sletting av tegn som matcher et mønster"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Hva og Hvorfor?
Å slette tegn som matcher et visst mønster er en vanlig oppgave for programmører. Dette kan hjelpe med å rydde opp i data og gjøre søk og filtrering mer effektivt.

# Slik gjør du det:
Et eksempel på å slette tegn ved hjelp av et mønster i C++ kan være som følger:

```C++
#include <iostream>
#include <regex>
using namespace std;

int main()
{
    string s = "Hei, dette er en test!";
    regex pattern("[,!]"); // mønsteret vi ønsker å matche
    cout << regex_replace(s, pattern, "") << endl; // bruker regex_replace funksjonen for å erstatte matchende tegn med ingenting
    return 0;
}
```

Dette vil outputte "Hei dette er en test" uten tegnene "," og "!".

# Dypdykk:
God kjennskap til regulære uttrykk (regex) er viktig for å kunne slette tegn ved hjelp av mønstre i C++. Regex tillater å finne og manipulere tekst basert på spesifikke tegnstrenger eller mønstre. Alternativt kan man bruke for eksempel strengmanipulasjonsfunksjoner som find og erase i stedet for regex.

# Se også:
- https://www.cplusplus.com/reference/regex/
- https://www.geeksforgeeks.org/regex-in-c-set-1/
- https://www.regular-expressions.info/