---
title:                "Skriver en tekstfil"
html_title:           "C++: Skriver en tekstfil"
simple_title:         "Skriver en tekstfil"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Skriving av en tekstfil kan være nyttig for å lagre strukturerte data eller for å lage enkel dokumentasjon for programmet ditt.

## Slik gjør du det

For å skrive en tekstfil i C++ trenger du å inkludere fstream biblioteket og å åpne en strøm til filen du ønsker å skrive til. Deretter kan du bruke << operator for å skrive data til filen, som vist i eksempelet nedenfor:

```C++
#include <fstream>
#include <iostream>

using namespace std;

int main() {
  // Åpner en strøm til tekstfilen "eksempel.txt"
  ofstream fil{"eksempel.txt"};

  // Skriver tekst til filen ved hjelp av << operator
  fil << "Dette er en tekstfil. \n";
  fil << "Her kan du skrive noe tekst og lagre den. \n";
  fil << "Du kan også inkludere variabler, som for eksempel: " << 10 << " eller " << "true \n";

  // Lukker filstrømmen når du er ferdig
  fil.close();

  return 0;
}
```

Når programmet kjøres vil det opprette en tekstfil kalt "eksempel.txt" og skrive følgende innhold til den:

```
Dette er en tekstfil.
Her kan du skrive noe tekst og lagre den.
Du kan også inkludere variabler, som for eksempel: 10 eller true
```

## Dypdykk

Hvis du ønsker å skrive mer komplekse data til en fil, kan du bruke C++'s iomanip bibliotek for å formatere utgangen. Du kan også bruke løkker og betingelser for å skrive en liste eller tabell til filen. Husk å alltid lukke filstrømmen når du er ferdig for å sikre at dataen blir skrevet riktig.

## Se også

- [C++ dokumentasjon](https://www.cplusplus.com/doc/)
- [C++ ressurser hos W3Schools](https://www.w3schools.com/cpp/)
- [Eksempel på å lese fra en tekstfil i C++](https://www.programiz.com/cpp-programming/examples/read-file)