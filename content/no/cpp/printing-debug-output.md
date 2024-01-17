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

# Hva & Hvorfor?
Printing debug output er en vanlig praksis blant programmere for å spore og fikse feil under utvikling av et program. Dette innebærer å skrive ut informasjon om variabler, progresjon av koden og eventuelle feilmeldinger.

Det å printe debug output hjelper programmere med å finne og løse feil raskere, og sikrer at programmet fungerer som forventet.

# Hvordan å:
For å printe debug output i C++, kan du bruke funksjonen `cout` fra standardbiblioteket `iostream`. Her er et eksempel på hvordan du kan printe verdien av variabelen `x`:

```C++
int x = 5;
cout << "Verdien av x er: " << x << endl;
```

Dette vil gi følgende output:
```
Verdien av x er: 5
```

Du kan også bruke syntaksen `printf` fra C, men dette er ikke anbefalt i moderne C++.

# Dypdykk:
Det å printe debug output har vært en vanlig praksis siden begynnelsen av programmering. Før i tiden, måtte programmere bruke devices som printere eller monitorer for å se debug output. Nå kan vi enkelt få tilgang til denne informasjonen gjennom konsoll output.

Noen alternativer til å printe debug output inkluderer bruk av debugging verktøy som GDB og Valgrind, eller å bruke en logging framework som log4cpp. Disse alternativene kan være mer omfattende og krever mer kunnskap for å bruke effektivt.

Når det kommer til implementasjon, er det viktig å ikke ha for mye debug output i produksjonskoden. Dette kan skade ytelsen til programmet og gjøre debugging vanskeligere.

# Se også:
- [C++ iostream](https://en.cppreference.com/w/cpp/header/iostream)
- [GDB - GNU Debugger](https://www.gnu.org/software/gdb/)
- [Valgrind](http://valgrind.org/)
- [log4cpp](https://github.com/richelbilderbeek/log4cpp)