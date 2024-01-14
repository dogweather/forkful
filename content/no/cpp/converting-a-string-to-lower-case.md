---
title:                "C++: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Dersom du jobber med string manipulasjon eller bruker input fra brukere, kan det være nyttig å konvertere alle bokstaver til små bokstaver (lower case). Dette gjør det enklere å håndtere og sammenligne tekster.

## Hvordan

For å konvertere en string til lower case i C++, kan du bruke funksjonen ```tolower()``` fra standardbiblioteket ```<cctype>```. Her er et enkelt eksempel:

```
#include <iostream>
#include <cctype>

using namespace std;

int main() {
  string tekst = "ALLE STORE BOKSTAVER";
  
  // looper gjennom alle tegn i stringen
  for (char& c : tekst) {
    // sjekker om tegnet er en stor bokstav
    if (isupper(c)) {
      // konverterer til lower case
      c = tolower(c);
    }
  }
  
  // skriver ut den konverterte stringen
  cout << tekst << endl;
  
  return 0;
}
```

**Output:**

```
alle store bokstaver
```

Det er også mulig å bruke funksjonen ```transform()``` fra standardbiblioteket ```<algorithm>``` for å konvertere en string til lower case. Her er et eksempel med input fra brukeren:

```
#include <iostream>
#include <algorithm>

using namespace std;

int main() {
  string tekst;
  
  // får input fra brukeren
  cout << "Skriv inn en string: ";
  getline(cin, tekst);
  
  // konverterer til lower case
  transform(tekst.begin(), tekst.end(), tekst.begin(), ::tolower);
  
  // skriver ut den konverterte stringen
  cout << tekst << endl;
  
  return 0;
}
```

**Output:**

```
skriv inn en string: JEG ER EN BRUKERINPUT
jeg er en brukerinput
```

## Deep Dive

Når du bruker funksjonen ```tolower()``` eller ```transform()```, vil alle tegn i stringen bli konvertert til lower case, inkludert spesialtegn og tall. Det er også viktig å huske at disse funksjonene ikke bare konverterer store bokstaver til små, men også bokstaver i andre språk og alfabet. Det finnes også andre måter å konvertere en string til lower case på, som å bruke en løkke og sjekke hvert tegn individuelt. Det er opp til programmeren å velge den metoden som best passer for deres behov.

## Se også

- [C++ string manipulation](https://www.programiz.com/cpp-programming/string)
- [C++ standard library](https://www.cplusplus.com/reference/)