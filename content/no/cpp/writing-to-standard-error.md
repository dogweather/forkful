---
title:                "C++: Skriving til standard feil"
simple_title:         "Skriving til standard feil"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Hvorfor skrive til standardfeil

Å skrive til standardfeil kan være et nyttig verktøy for å få informasjon ut av programmet ditt som ikke skal vises til brukeren. Dette kan inkludere feilmeldinger, debugging informasjon eller annen viktig informasjon som kan hjelpe deg med å forstå og forbedre koden din.

## Slik gjør du det

Hvis du vil skrive til standardfeil i C++, kan du bruke std::cerr-objektet. Dette vil skrive ut informasjon på standard feilstrøm, som standard blir sendt til konsollen din.

```C++
#include <iostream>

int main() {
    std::cerr << "Dette er en feilmelding" << std::endl;
    return 0;
}
```

Dette vil resultere i følgende utdata:

```
Dette er en feilmelding
```

Du kan også kombinere dette med forskjellige variabler eller uttrykk for å skrive ut mer dynamisk informasjon:

```C++
#include <iostream>

int main() {
    int x = 5;
    std::cerr << "Verdien av x er: " << x << std::endl;
    return 0;
}
```

Dette vil resultere i følgende utdata:

```
Verdien av x er: 5
```

## Dypdykk

En annen måte å skrive til standardfeil på er å bruke std::errbuf-objektet. Dette vil skrive ut informasjon til en buffer som deretter kan bli hentet ut og brukt senere i programmet.

```C++
#include <iostream>

int main() {
    std::errbuf << "Dette vil bli lagret i bufferen" << std::endl;

    // koden din fortsetter her

    std::cerr << std::errbuf; // henter ut bufferen og skriver til standardfeil
    return 0;
}
```

Dette kan være nyttig når du trenger å loggføre informasjon eller når du må skrive ut informasjon senere i koden din.

# Se også

- [C++ Standard Library](https://www.cplusplus.com/reference/) for mer informasjon om std::cerr og std::errbuf.
- [Feilhåndtering i C++: try, catch, throw](https://www.w3schools.com/cpp/cpp_exceptions.asp) for å lære om hvordan du kan håndtere og skrive ut feil i C++.