---
title:                "Å finne lengden på en streng"
html_title:           "C++: Å finne lengden på en streng"
simple_title:         "Å finne lengden på en streng"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å finne lengden på en streng er en vanlig oppgave for programmerere. Dette betyr ganske enkelt å telle antall tegn i en streng, inkludert mellomrom og spesialtegn. Dette er nyttig for å håndtere tekstbaserte data og for å sikre at strenger ikke er for lange til å bli behandlet riktig.

## Hvordan:
```C++
// Eksempel på å finne lengden på en streng i C++
#include <iostream>
#include <string>
using namespace std;

int main() {
  string navn = "Eirik";
  int lengde = navn.length(); // bruker .length() metoden på strengen
  cout << "Lengden på navnet mitt er " << lengde << " tegn.";
  return 0;
}

// Eksempel på output:
// Lengden på navnet mitt er 5 tegn.
```

## Dykk dypere:
Å finne lengden på en streng er en viktig del av tekstbehandling i programmering. Dette begrepet har sitt opphav fra ASCII kodingen, som brukte spesielle tegn for å markere starten og slutten på en streng. I tillegg til å bruke .length() metoden, kan du også bruke funksjonen strlen() for å finne lengden på en streng i C. Det finnes også alternative måter å telle antall tegn på, som for eksempel å bruke løkker og telle hver bokstav individuelt.

## Se også:
- [C++ Strings](https://www.programiz.com/cpp-programming/string) (engelsk)
- [Historien om ASCII-koden](https://www.livescience.com/33179-origin-computer-keyboard-design.html) (engelsk)
- [strlen() funksjonen i C](https://www.tutorialspoint.com/c_standard_library/c_function_strlen.htm) (engelsk)