---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Arduino: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å konvertere en streng til små bokstaver betyr å endre alle hovedbokstavene i strengen til små bokstaver. Programmerere gjør dette for enklere sammenligning og manipulering av tekstdata, spesielt i søkealgoritmer og datarensing.

## Hvordan :

Her er et enkelt eksempel på hvordan du konverterer en streng til små bokstaver i C++.

```C++
#include <iostream> 
#include <algorithm> 
#include <string> 
using namespace std;

int main() 
{
    string str = "HELLO, WORLD!";
    
    transform(str.begin(), str.end(), str.begin(), ::tolower);
    
    cout << str; 

    return 0; 
}
```

Når dette programmet kjøres, vil det ha en utgang som:

```
hello, world!
```

## Dypdykk

Historisk har måten å konvertere en streng til små bokstaver på variert mellom ulike programmeringsspråk og biblioteker. I tidligere versjoner av C++ brukes for eksempel for-løkker og ASCII-verdier for å gjøre denne type operasjon. Med moderne standardbibliotek er denne prosessen forenklet og mer effektiv.

Alternativt finnes det mange andre måter å gjøre denne operasjonen på, som å bruke funksjonen `std::for_each` eller skrive din egen funksjon. Hvilken metode du velger, avhenger av situasjonen og programmeringsstilen din.

Implementeringsdetaljer spesielt for strenger i lavere rekkefølge i C++ involverer bruk av standardbibliotekets `std::transform`. Denne funksjonen kan brukes til å gjennomføre transformasjoner på en hvilken som helst STL-beholder. I tilfelle av strenger, tar den i bruk en funksjon kalt `::tolower`, som konverterer hver enkelt karakter i strengen til mindre bokstaver. 

## Se Også 

For mer informasjon om C++ og strengmanipulasjon:

- Standardbibliotekdokumentasjon om 'transform': https://en.cppreference.com/w/cpp/algorithm/transform
- Utforske C++ streng klasse: https://www.cplusplus.com/reference/string/string/
- Dybdykk inn i C++ algoritmer: https://www.learn-cpp.org/