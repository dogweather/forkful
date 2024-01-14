---
title:    "C++: Lagring av strenger"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Hvorfor

Å sette sammen strenger (concatenation) er en viktig og nyttig del av programmering. Det lar deg kombinere flere strenger til en lengre streng, noe som er spesielt nyttig i tekstbehandling og utskrift av data.

## Hvordan Gjøre Det

For å sette sammen strenger i C++, kan du bruke operatoren "+" eller funksjonen "concat" fra string biblioteket. Her er et eksempel:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
  string navn = "John";
  string etternavn = "Doe";
  string fulltNavn = navn + " " + etternavn;
  cout << "Hei, mitt navn er " << fulltNavn << endl;
  return 0;
}
```

I dette eksempelet, bruker vi operatoren "+" til å sette sammen strengene "navn" og "etternavn" til variabelen "fulltNavn." Merk at vi også inkluderer et mellomrom mellom navnene ved å legge til en tom streng (" ") mellom dem. Output av dette programmet vil være:

```
Hei, mitt navn er John Doe
```

I tillegg til å bruke operatoren "+", kan du også bruke funksjonen "concat" fra string biblioteket til å sette sammen strenger. Her er et eksempel:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
  string land = "Norge";
  string hilsen = "Halla";
  string melding = concat(hilsen, " fra ", land);
  cout << melding << endl;
  return 0;
}
```

I dette eksempelet, bruker vi funksjonen "concat" til å sette sammen strengene "hilsen", "fra" og "land" til variabelen "melding." Output av dette programmet vil være:

```
Halla fra Norge
```

## Dypdykk

I C++, kan du også bruke en annen metode for å sette sammen strenger kalt stringstream. Dette er spesielt nyttig hvis du må sette sammen en stor mengde strenger eller dynamisk sette sammen variabler og strenger.

For å bruke stringstream, må du inkludere header-filen "sstream" og opprette en stringstream-variabel. Deretter kan du bruke funksjonen "str" til å konvertere andre data typer til strenger og sette dem sammen med operatoren "+."

Her er et eksempel:

```C++
#include <iostream>
#include <sstream>
#include <string>

using namespace std;

int main() {
  int alder = 25;
  stringstream ss;
  ss << "Jeg er " << alder << " år gammel.";
  string utskrift = ss.str();
  cout << utskrift << endl;
  return 0;
}
```

I dette eksempelet, bruker vi stringstream til å sette sammen strengen "Jeg er" med variabelen "alder" og strengen "år gammel." Output av dette programmet vil være:

```
Jeg er 25 år gammel.
```

## Se Også

For mer informasjon om å sette sammen strenger i C++, sjekk ut disse ressursene:

- [Offisiell C++ Dokumentasjon - Strenger](https://en.cppreference.com/w/cpp/string)
- [Tutorialspoint - C++ Strenger](https://www.tutorialspoint.com/cplusplu