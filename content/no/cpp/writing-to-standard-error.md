---
title:                "C++: Skriver til standardfeil"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor?

Å skrive til standard error kan være nyttig i tilfeller der du ønsker å vise feil- og progressmeldinger til brukeren. Dette kan hjelpe deg med å feilsøke og forbedre koden din.

## Hvordan gjøre det?

For å skrive til standard error i C++, må du bruke biblioteket `iostream` og operatoren `<<`. Nedenfor er et eksempel på hvordan du kan skrive til standard error:

```C++
#include <iostream>
using namespace std;

int main() {
    cerr << "Dette er en feilmelding" << endl;
    cout << "Dette er en vanlig utskrift" << endl;
    
    return 0;
}
```

Output:

```
Dette er en feilmelding
Dette er en vanlig utskrift
```
Som du kan se, blir feilmeldingen skrevet til standard error ved hjelp av `cerr`, mens den vanlige utskriften blir skrevet til standard output ved hjelp av `cout`. Ved å skrive til standard error, skiller du disse to typene utskrifter og kan enklere identifisere feilene dine.

## Dykk dypere

Å skrive til standard error er en del av "streaming" konseptet i C++, der data sendes via en strøm av tegn. Denne strømmen kan enten være inn- eller ut-data, noe som gir deg fleksibilitet til å skrive til både standard output og error.

Det er også viktig å merke seg at når du bruker C++ på forskjellige plattformer, kan standard error vises annerledes. På Windows brukes en rød tekst med en "X" som feilmelding, mens på Unix og Linux brukes det røde tekst og ingen spesiell symbol. Derfor kan det være lurt å teste koden din på ulike plattformer for å sikre konsistent utskrift.

## Se også

- [Å skrive til standard error i C++ av GeeksforGeeks (engelsk)](https://www.geeksforgeeks.org/writing-to-file-and-error-console-in-cpp/)
- [Stream konseptet i C++ av cplusplus.com (engelsk)](http://www.cplusplus.com/doc/tutorial/files/)
- [Standard error på ulike plattformer av The C++ Programming Language (engelsk)](https://stackoverflow.com/questions/15175178/how-to-change-the-text-color-of-cerr-in-windows)