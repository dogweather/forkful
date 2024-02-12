---
title:                "Refaktorering"
aliases:
- no/cpp/refactoring.md
date:                  2024-01-26T01:17:09.888660-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktorering"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/refactoring.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Refaktorisering er prosessen med å endre den interne strukturen til et dataprogram uten å endre dets eksterne oppførsel. Programmerere gjør dette for å rydde opp i koden sin, noe som gjør den lettere å forstå, vedlikeholde og utvide.

## Hvordan:

Forestille deg at du har en funksjon som gjør litt for mye, som denne klumpete metoden som initialiserer et objekt og også utfører logging:

```C++
#include <iostream>

class Widget {
public:
    void init(bool verbose) {
        // Initialiseringslogikk
        // ...

        // Utførlig logging
        if (verbose) {
            std::cout << "Widget initialisert!" << std::endl;
        }
    }
};

// Bruk:
Widget w;
w.init(true);
```

Utdata:
```
Widget initialisert!
```

Å refaktorere dette til renere, mer fokuserte metoder kan se slik ut:

```C++
#include <iostream>

class Widget {
public:
    void init() {
        // Bare initialiseringslogikk
        // ...
    }

    void loggInitialisering() const {
        std::cout << "Widget initialisert!" << std::endl;
    }
};

// Bruk:
Widget w;
w.init();
w.loggInitialisering();
```

Denne endringen har ikke endret hva programmet gjør, men gjør `Widget`-klassen mer modulær og bruken mer klar.

## Dypdykk

Konseptet om refaktorisering slik vi kjenner det i dag har sine røtter i Smalltalk-programmeringsfellesskapene på 1980-tallet og ble sterkt popularisert av Martin Fowlers bok "Refactoring: Improving the Design of Existing Code" fra 1999. I dag er refaktorisering en kjernekomponent i moderne programvareutvikling, integrert i ulike utviklingsmetodologier som Agile og TDD (Test-Drevet Utvikling).

Når vi snakker om alternativer til refaktorisering, beveger vi oss inn på området for omskriving eller redesign. Refaktorisering er strategisk og trinnvis, mens en omskrivning kan skrote eksisterende kode til fordel for en ny løsning. Redesign, på den andre siden, kan innebære mer betydningsfulle endringer inkludert endring av funksjonalitet, som er et ikke-mål for ren refaktorisering.

Gjennomføringsdetaljer om refaktorisering kan bli ganske detaljert. Det er mange 'kode lukter' som kan tilsi en refaktor, som lange metoder, store klasser eller duplisert kode. Automatiserte verktøy eksisterer som kan assistere i refaktorisering, som "Clang-Tidy" for C++, som kan identifisere problemer og til og med anvende noen løsninger.

Dessuten krever refaktorisering en solid samling av tester for å sikre at funksjonaliteten forblir uendret. Uten tester flyr du i praksis blindt og risikerer regress.

## Se Også

For en dypere forståelse av refaktorisering og for å se flere eksempler, vil du kanskje sjekke ut:

- Martin Fowlers klassiske tekst "Refactoring: Improving the Design of Existing Code" for grunnleggende ideer og strategier.
- `Clang-Tidy`-dokumentasjonen på https://clang.llvm.org/extra/clang-tidy/ for automatisert refaktoriseringsstøtte i C++.
- "Working Effectively with Legacy Code" av Michael Feathers, som gir teknikker for trygg refaktorisering i konteksten av mindre enn perfekte eksisterende kodebaser.
