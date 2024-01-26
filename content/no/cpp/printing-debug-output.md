---
title:                "Skrive ut feilsøkingsdata"
date:                  2024-01-20T17:52:02.397871-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skrive ut feilsøkingsdata"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Utskrift av feilsøkingsdata hjelper utviklere med å forstå hva som skjer i koden deres. Det er en enkel metode for å spore variabler og applikasjonsflyt uten komplekse verktøy.

## Slik gjør du:
```C++
#include <iostream>

int main() {
    int a = 5;
    int b = 10;
    int sum = a + b;
    
    // Skriver ut summen til konsollet for feilsøking
    std::cout << "Summen er: " << sum << std::endl;

    return 0;
}
```
Sample output:
```
Summen er: 15
```

## Dypdykk
Før `iostream`, brukte vi `printf` fra C-standardbiblioteket. Alternativer idag inkluderer loggbiblioteker og innebygde debuggerverktøy. Å skrive ut til konsollen er rått, men det gir umiddelbar innsikt.

Implementeringen av utskrift i C++ er håndtert av `iostream` biblioteket med operatøren `<<` til å sende data til output-strømmen, som vanligvis er konsollen.

## Se også:
- Cppreference om [`iostream`](https://en.cppreference.com/w/cpp/header/iostream)
- En guide til moderne C++ debugging: [The Definitive Guide to C++ Debugging](https://www.jetbrains.com/help/clion/debugging-code.html)
- For mer innsikt i C-standardbibliotekets `printf`: [cppreference.com: cstdio](https://en.cppreference.com/w/c/io/fprintf)
