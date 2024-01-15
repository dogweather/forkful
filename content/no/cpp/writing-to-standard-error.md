---
title:                "Skriving til standardfeil"
html_title:           "C++: Skriving til standardfeil"
simple_title:         "Skriving til standardfeil"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Så du har akkurat begynt å lære programmering, og du har hørt om standard error, men du lurer på hvorfor du skulle bry deg om å skrive til det? Vel, her er to grunner: 

1. Feilsøking: Skriv til standard error for å feilsøke og finne ut hva som går galt i koden din. Dette gir deg mer spesifikk informasjon om feilene dine, som kan hjelpe deg med å fikse dem raskere.
2. Enkelhet: Det er enklere å skrive til standard error enn å skrive til standard utgang. Å skrive til standard utgang krever flere trinn for å få ut riktig format, mens skriving til standard error beholder koden din i sin opprinnelige form.

## Hvordan gjøre det

For å skrive til standard error i C++, bruker vi objektet ```cerr``` og << operatøren. La oss se på et eksempel:

```
#include <iostream>

using namespace std;

int main() {
    int num = 5;
    cerr << "Det høres ut som at du har problemer med num = " << num << endl;
    
    return 0;
}
```
Output:
```
Det høres ut som at du har problemer med num = 5
```

Her har vi brukt 
objektet ```cerr``` og << operatøren til å skrive en feilmelding. Merk at vi også inkluderer ```<iostream>``` og bruker ```using namespace std;``` for å skrive ut feilmeldingen.

## Dypdykk

Når vi skriver til standard error, bruker vi ofte begrepet "stderr stream". Dette er bare navnet på strømmen som brukes til standard error, akkurat som cin og cout er navnene på strømmene som brukes til henholdsvis standard input og standard utgang.

Det er også viktig å merke seg at stderr stream er uavhengig av standard output stream (cout). Dette betyr at du kan skrive til stderr og cout i samme program uten å måtte bekymre deg for å blande dem sammen.

## Se også

- [C++ Dokumentasjon - Standard Error](https://www.cplusplus.com/reference/cstdio/fprintf/?kw=stderr)
- [Wikipedia - Standard streams](https://en.wikipedia.org/wiki/Standard_streams)
- [Guide til feilhåndtering i C++](https://www.programiz.com/cpp-programming/error-handling)