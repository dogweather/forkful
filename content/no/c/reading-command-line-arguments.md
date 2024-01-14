---
title:    "C: Lesing av kommandolinje-argumenter"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Hvorfor
Å forstå hvordan man leser kommandolinjeargumenter er viktig for å kunne skrive effektiv og brukervennlig kode. Ved å lese argumenter fra kommandolinjen, kan man lage programmer som er fleksible og tilpassbare basert på brukerens behov.

# Hvordan gjøre det
For å lese kommandolinjeargumenter i C, må man bruke funksjonen `main()` og dens parametere `argc` og `argv`. `argc` inneholder det totale antallet argumenter som blir sendt med, mens `argv` er en liste av strenger som inneholder de faktiske argumentene.

La oss se på et enkelt eksempel for å illustere dette:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("Antall argumenter: %d\n", argc);
    for (int i = 0; i < argc; i++) {
        printf("Argument %d: %s\n", i, argv[i]);
    }
    return 0;
}
```

La oss anta at vi kompilerer og kjører dette programmet med følgende kommandolinje-argumenter:

```bash
gcc example.c -o example
./example hello world
```

Outputen vil da være:

```
Antall argumenter: 3
Argument 0: ./example
Argument 1: hello
Argument 2: world
```

Her ser vi at `argc` er 3, og at `argv` inneholder de tre argumentene vi sendte med. For å kunne bruke argumentene i koden vår, kan vi for eksempel lagre dem i variabler og deretter gjøre operasjoner med dem.

# Dypdykk
Nå som vi har sett et enkelt eksempel på hvordan man kan lese kommandolinjeargumenter i C, kan vi se litt dypere på hvordan dette fungerer. `argc` og `argv` er definert i headerfilen `stdlib.h`. `argv` er en peker til en peker (en liste av strenger), og alle strengene i denne listen er null-terminerte, noe som betyr at de avsluttes med `\0`-tegnet.

Det finnes også en annen måte å lese kommandolinjeargumenter på, ved bruk av funksjonen `getopt()`. Dette kan være nyttig dersom man har behov for en mer fleksibel argumentbehandling, men kan være litt mer avansert å implementere.

# Se også
- [How To Use Command Line Arguments in C](https://www.digitalocean.com/community/tutorials/how-to-use-arguments-and-parameters-in-a-c-program)
- [The getopt() function in C](https://www.gnu.org/s/libc/manual/html_node/Example-of-Getopt.html)