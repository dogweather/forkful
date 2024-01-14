---
title:    "C: Lesing av kommandolinjeargumenter"
keywords: ["C"]
---

{{< edit_this_page >}}

# Hvorfor

Å lese kommandolinjeargumenter i C-programmering kan være en viktig ferdighet for å lage mer fleksible, dynamiske og brukervennlige programmer. Ved å la brukere gi inn egne argumenter via kommandolinjen, kan man gi muligheten til å endre funksjonaliteten til et program uten å måtte endre koden. Dette kan også gjøre det enklere for brukere å tilpasse programmet til sine egne behov.

# Hvordan

Å lese kommandolinjeargumenter i C er enkelt og kan gjøres ved hjelp av `argc` og `argv`-variablene. `argc` inneholder antall argumenter som er gitt, mens `argv` er en array som inneholder verdiene til argumentene. La oss se på et eksempel:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {

    // Skriver ut alle kommandolinjeargumentene
    for (int i = 0; i < argc; i++) {
        printf("Argument %d: %s\n", i, argv[i]);
    }

    return 0;
}
```

Hvis dette programmet kalles med følgende kommandolinje:

```
./program arg1 arg2 arg3
```

Vil det skrive ut:

```
Argument 0: ./program
Argument 1: arg1
Argument 2: arg2
Argument 3: arg3
```

Plasseringen av argumentene i `argv` avhenger av operativsystemet, men det første argumentet vil alltid være selve programnavnet.

# Dykke dypere

For å lese kommandolinjeargumentene på en mer avansert måte, kan man bruke `getopt`-funksjonen. Denne funksjonen lar deg spesifisere hvilke argumenter som er påkrevd og hvilke som er valgfrie, samt angi hva slags handling som skal utføres når spesifikke argumenter er gitt. For mer informasjon og eksempler på bruk av `getopt`, sjekk ut denne guiden: [https://www.gnu.org/software/libc/manual/html_node/Example-of-Getopt.html](https://www.gnu.org/software/libc/manual/html_node/Example-of-Getopt.html).

# Se også

- [https://www.tutorialspoint.com/cprogramming/c_command_line_arguments.htm](https://www.tutorialspoint.com/cprogramming/c_command_line_arguments.htm)
- [https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [https://www.learn-c.org/en/Command_Line_Arguments](https://www.learn-c.org/en/Command_Line_Arguments)