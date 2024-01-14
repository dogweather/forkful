---
title:    "C++: Utskrift av feilrettingsutdata"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvis du noen gang har programmert i C++, har du nok også støtt på behovet for å finne og feilsøke potensielle problemer i koden din. Å printe debug-utdata er en enkel, men effektiv måte å få mer informasjon om hva som skjer i programmet ditt, noe som kan hjelpe deg med å finne og fikse feil raskere.

## Hvordan
For å printe debug-utdata i C++, kan du bruke en av to metoder: `cout` eller `cerr`. Begge disse er en del av `iostream` biblioteket, så du må inkludere dette i koden din først.

```C++
#include <iostream>
```

Deretter kan du bruke `cout` for å skrive ut meldinger til standard output (oftest skjermen):

```C++
cout << "Dette er en debug-melding" << endl;
```

Du kan også bruke `cerr` for å skrive til standard error stream, som også er skjermen, men brukes hovedsakelig for feilmeldinger.

```C++
cerr << "Dette er en feilmelding" << endl;
```

Det er også mulig å inkludere variabler i debug-utdataen, for å få mer spesifikk informasjon om hva som skjer i koden din.

```C++
int a = 10;
cout << "Variabel a har verdien: " << a << endl;
```

Dette vil printe ut følgende:

```
Variabel a har verdien: 10
```

## Dykk dypere
Å bruke `cout` og `cerr` er en enkel og rask måte å få debug-informasjon på, men det er også noen ting du bør være klar over når du bruker det. For det første, vil `cout` og `cerr` begge skrive ut til standard output eller error stream, uavhengig av hvilken tråd som kaller dem. Dette kan føre til uforutsette utskrifter hvis du for eksempel har flere tråder som kjører samtidig.

Det er også viktig å være nøye med å fjerne debug utdata når koden din er ferdig og klar for produksjon. Å la debug-utdata stå igjen kan føre til unødvendig output og påvirke ytelsen til programmet ditt.

## Se også
- [C++ Debugging Techniques](https://docs.microsoft.com/en-us/visualstudio/debugger/cpp-debugging-techniques)
- [Debugging C++ code in Visual Studio](https://www.youtube.com/watch?v=Rd3IGXo8xUI)
- [Effektiv feilsøking i C++](https://www.udemy.com/course/effektiv-feilsoking-i-c/)