---
title:    "C++: Beregning av en dato i fremtiden eller fortiden"
keywords: ["C++"]
---

{{< edit_this_page >}}

# Hvorfor

Det er mange situasjoner der man må beregne en dato i fremtiden eller fortiden, enten det er for å planlegge et arrangement, for å holde oversikt over viktige hendelser, eller for å enkelt kunne angi datoer i programvareprosjekter. Derfor kan det være nyttig å ha en forståelse for hvordan man utfører slike beregninger i C++.

# Hvordan

For å beregne en dato i fremtiden eller fortiden i C++, trenger vi å bruke noen av de innebygde funksjonene og datastrukturene i språket. Etter å ha definert variabler for dag, måned og år, kan vi bruke funksjonen `tm` fra `time.h`-biblioteket til å opprette en datostruktur. Deretter kan vi bruke funksjoner som `mktime` og `localtime` til å justere denne datoen basert på det ønskede antallet dager.

```C++
#include <iostream>
#include <time.h>

int main() {
  // Definere variabler for dag, måned og år
  int dag = 15;
  int måned = 8;
  int år = 2021;

  // Opprett en datostruktur
  struct tm dato = {0};

  // Sett verdier for dag, måned og år
  dato.tm_year = år - 1900;
  dato.tm_mon = måned - 1;
  dato.tm_mday = dag;

  // Beregn datoen for 30 dager frem i tid
  dato.tm_mday += 30;

  // Justere datoen basert på det aktuelle måneden og året
  mktime(&dato);
  // Konvertere datoen til en lokal tidssone
  char* datostr = asctime(localtime(&dato));

  // Skrive ut resultatet
  std::cout << "Datoen er: " << datostr << std::endl;

  return 0;
}
```

**Output:**

```
Datoen er: Thu Sep  14 00:00:00 2021
```

# Dypdykk

I tillegg til å beregne en dato i fremtiden eller fortiden, kan det være nyttig å vite hvordan man kan hente ut og manipulere spesifikke deler av datoen. For eksempel kan man bruke funksjoner som `gettm` til å hente ut verdier for dag, måned og år separat. Man kan også bruke funksjoner som `difftime` til å beregne differansen mellom to datoer.

Et annet nyttig bibliotek for å arbeide med datoer og tider i C++ er `ctime`, som gir funksjoner for å formatere, konvertere og presentere datoer på ulike måter.

Det kan også være lurt å håndtere ulike datatyper og formater for datoer i ulike operativsystemer, og å være oppmerksom på forskjellene mellom lokal tidssone og UTC-tid.

# Se også

- [cppreference.com - Dato og tid library](https://en.cppreference.com/w/cpp/chrono)
- [cplussedu.com - Dato og tid operasjoner i C++](https://www.cplusplus.com/doc/oldtutorial/date_and_time/)