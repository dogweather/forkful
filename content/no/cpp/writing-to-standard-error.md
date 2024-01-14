---
title:    "C++: Skriving til standardfeil."
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standard error i C++ kan virke som en unødvendig prosess, men det kan være svært nyttig når du skal finne og rette feil i koden din. Ved å utskrive feilmeldinger til standard error i stedet for standard output, kan du enkelt skille mellom regulær utskrift og feilmeldinger.

## Slik gjør du det

Det er enkelt å skrive til standard error i C++. Du trenger bare å inkludere biblioteket "iostream" og deretter bruke kommandoen "std::cerr << "Feilmelding" << std::endl;" for å skrive til standard error. Du kan også bruke "std::cerr" flere ganger i koden din for å skrive ut flere feilmeldinger.

```C++
#include <iostream>
using namespace std;

int main() {
    // En feilmelding som skal skrives til standard error
    std::cerr << "Dette er en feilmelding!" << std::endl;
    
    // Annen kode og utskrift som skal skrives til standard output
    std::cout << "Dette er en vanlig utskrift." << std::endl;
 
    return 0;
}
```

Output:
```
Dette er en feilmelding!
Dette er en vanlig utskrift.
```

## Dypdykk

Når vi bruker standard error til å skrive ut feilmeldinger, er det viktig å huske på at dette kun er nyttig når programmet kjøres i en terminal eller kommandolinje. Hvis du kjører programmet ditt fra et grafisk brukergrensesnitt, vil feilmeldingene bli skjult og du vil ikke kunne se dem.

En annen viktig ting å huske på er at standard error også kan kobles til en fil, slik at du kan lagre feilmeldingene dine og se dem senere. Dette kan være nyttig når du jobber med større prosjekter og trenger å undersøke feil på et senere tidspunkt.

## Se også

- [C++ iostream bibliotek dokumentasjon](http://www.cplusplus.com/reference/iostream/)
- [Hvordan løse C++ feilmedlinger](https://www.cplusplus.com/articles/yU6vU7Lw/)
- [Hva er forskjellen mellom standard output og standard error?](https://stackoverflow.com/questions/8355844/what-is-the-difference-between-stdcout-stdcerr-and-stdclog)