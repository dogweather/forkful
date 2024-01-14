---
title:    "C++: Lesing av kommandolinjeargumenter"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor
Å lese kommandolinjeargumenter er en viktig ferdighet for enhver programmerer. Det lar deg skrive programmer som er mer fleksible og interaktive, og som kan ta imot forskjellige input fra brukeren.

## Hvordan
For å lese kommandolinjeargumenter i C++, må du inkludere `#include <iostream>` i koden din. Deretter kan du bruke funksjonen `int main(int argc, char* argv[])` for å lese argumentene. Et eksempel på en enkel implementering kan se slik ut:

```C++
#include <iostream>
using namespace std;

int main(int argc, char* argv[]){
  for (int i = 0; i < argc; i++){
    cout << "Argument #" << i << ": " << argv[i] << endl;
  }
  return 0;
}
```

I dette eksempelet bruker vi en løkke for å skrive ut alle kommandolinjeargumentene som ble gitt til programmet. For eksempel, hvis vi kjørte programmet med kommandolinjeargumentene `argument1 argument2 argument3`, så ville outputen vært:
```
Argument #0: programnavn.exe
Argument #1: argument1
Argument #2: argument2
Argument #3: argument3
```
Vær oppmerksom på at kommandolinjeargumentene alltid begynner med `argv[1]`, da `argv[0]` alltid inneholder navnet på programmet.

## Deep Dive
Hvis du vil gå dypere inn i lesing av kommandolinjeargumenter, er det viktig å forstå at `argc` representerer antall argumenter gitt, og `argv` er et array som inneholder alle argumentene. Du kan også bruke `string` datatypeen istedenfor `char*` for en mer fleksibel implementering.

Det er også mulig å sjekke om et bestemt argument ble gitt eller ikke, ved å bruke en `if`-setning. For eksempel, hvis vi ønsker å sjekke om brukeren ga `argument2` som et argument, kan vi bruke følgende kode inne i `for`-løkken vår:
```C++
if (string(argv[i]) == "argument2"){
  cout << "Brukeren ga argument2 som argument!" << endl;
}
```

## Se også
- [C++ Kommandolinjeargumenter - cplusplus.com](https://www.cplusplus.com/articles/DEN36Up4/)
- [C Strings og `string` konvertering - cplusplus.com](https://www.cplusplus.com/articles/D9j2Nwbp/)