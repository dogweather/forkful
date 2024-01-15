---
title:                "Utskrift av feilsøkingsutdata"
html_title:           "C++: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Det kan være mange fordeler ved å skrive ut debug-utdata i C++. Det kan hjelpe deg med å finne feil i koden din og finjustere ytelsen på programmet. Det kan også gi deg en bedre forståelse av hvordan koden din fungerer og hvordan ulike variabler og metoder samhandler.

## Hvordan gjøre det

For å skrive ut debug-utdata i C++, kan du bruke funksjonen `cout` i `iostream` biblioteket. Her er et eksempel på hvordan du kan skrive ut en variabel `x`:

```C++
#include <iostream>
using namespace std;

int main() {
    int x = 5;
    cout << "x = " << x << endl;
    return 0;
}
```

Dette vil skrive ut "x = 5" i terminalen når du kjører programmet ditt. Du kan også skrive ut mer kompleks utdata som for eksempel en liste med elementer:

```C++
#include <iostream>
using namespace std;

int main() {
    int numbers[] = {1, 2, 3, 4, 5};
    for (int i = 0; i < 5; i++) {
        cout << numbers[i] << " ";
    }
    return 0;
}
```

Dette vil skrive ut "1 2 3 4 5" når du kjører programmet ditt.

## Dypdykk

Hvis du ønsker å skrive ut debug-utdata for å få en dypere forståelse av koden din, kan du også legge til ekstra informasjon i utdataen din. For eksempel kan du skrive ut verdien av en variabel for hver iterasjon av en løkke for å se hvordan verdien endres. Dette kan være nyttig for å finne feil i koden eller for å forstå hvordan noe fungerer.

En annen nyttig teknikk er å bruke `assert` funksjonen for å sjekke at en antagelse i koden din er sann. Hvis du antar at en variabel aldri vil være negativ, kan du legge til en `assert` statement for å sjekke at dette er tilfelle. Hvis statementen ikke blir oppfylt, vil programmet ditt avslutte og gi deg en feilmelding som kan hjelpe deg med å finne og løse feilen din.

## Se også

Her er noen nyttige lenker for å lære mer om å skrive ut debug-utdata i C++:

- [C++ Tutorial - Debug Output](https://www.learncpp.com/cpp-tutorial/todays-lesson-output-debugging/)
- [The Art of Debugging With GDB, DDD, and Eclipse](https://www.youtube.com/watch?v=X1jWe5rOu3g)
- [Debugging Your C++ Program Using GDB](https://www.tutorialsteacher.com/cpp/cpp-debugging-with-gdb)