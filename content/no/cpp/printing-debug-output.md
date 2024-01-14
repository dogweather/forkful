---
title:    "C++: Utskrift av feilsøkingsutdata"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Hvorfor

Feilfeilsøking er en viktig del av C++ programmering, og for å effektivt finne og rette feil, er det ofte nødvendig å printe ut debug informasjon i koden. Dette lar utviklere se hva som skjer under kjøring av programmet og kan hjelpe med å identifisere og løse problemer.

## Hvordan

For å printe debug informasjon i C++, bruker vi ```cout``` kommandoen. For å gjøre dette trenger vi å inkludere ```iostream``` biblioteket med følgende kode:

```C++
#include <iostream>
```

Deretter kan vi bruke ```cout``` kommandoen for å printe ut en melding eller variabel til konsollen. For eksempel:

```C++
#include <iostream>

int main() {
    int num = 5;
    cout << "Variabelens verdi er: " << num << endl;
    return 0;
}
```

Dette vil printe ut følgende i konsollen:

```Variabelens verdi er: 5```

Vi kan også legge til egne meldinger eller variabler ved å bruke ```<<``` operatøren og avslutte med ```endl```. Dette kan gjentas flere ganger for å printe ut flere linjer av debug informasjon.

## Dypdykk

Det er viktig å være klar over at å bruke for mye debug output kan påvirke ytelsen til programmet ditt. Derfor er det viktig å være selektiv med hva som blir printet ut og kanskje legge til en mulighet til å skru av debug output når programmet lanseres.

En annen nyttig måte å printe ut debug informasjon på er med ```cerr``` kommandoen. Denne fungerer på samme måte som ```cout```, men sender output til std. error i stedet for std. out. Dette er nyttig for å skille mellom debug informasjon og vanlig output når begge blir printet til konsollen.

## Se også

- [Hvordan Debugge i C++](https://www.digitalocean.com/community/tutorials/how-to-debug-in-cpp-using-gdb) av DigitalOcean
- [Effektiv Feilfeilsøking i C++](https://hackernoon.com/effective-debugging-in-c-cpp-a147f6778fc1) av Hacker Noon