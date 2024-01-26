---
title:                "Å bruke en feilsøker"
date:                  2024-01-26T03:47:40.105103-07:00
model:                 gpt-4-0125-preview
simple_title:         "Å bruke en feilsøker"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/using-a-debugger.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
En feilsøker er et verktøy som lar deg inspisere C-koden din mens den kjører, skritt for skritt, for å finne feil. Programmerere bruker feilsøkere for å forstå hvordan koden deres oppfører seg, rette feil og optimalisere ytelsen uten å gjette.

## Hvordan:
Si at du jobber med et enkelt C-program som kalkulerer fakultetet til et tall, men det er en glitch. For å bruke en feilsøker som `gdb` (GNU Debugger), kompiler først med `-g`-flagget for å inkludere feilsøkingsinfo:

```c
// kompiler med: gcc factorial.c -o factorial -g
#include <stdio.h>

long factorial(int n) {
    if (n < 0) return 0; // En enkel sjekk for negativt input
    long result = 1;
    while (n > 1)
        result *= n--;
    return result;
}

int main() {
    int nummer = 5;
    long resultat = factorial(nummer);
    printf("Fakultetet av %d er %ld\n", nummer, resultat);
    return 0;
}
```

Deretter kjører du den i gdb:

```shell
$ gdb ./factorial
```

Sett et brytepunkt ved `factorial`-funksjonen og kjør programmet:

```gdb
(gdb) break factorial
(gdb) run
```

Når det treffer brytepunktet, steg gjennom hver linje med `next` eller `n` og inspiser variabler med `print` eller `p`:

```gdb
(gdb) next
(gdb) print resultat
$1 = 1
```

Eksempelutdata vil gi sanntidsverdier og programutførelsesflyt.

## Dypdykk
Feilsøkere har vært rundt siden 1960-tallet, utviklet fra enkle monitorer til komplekse, GUI-baserte applikasjoner. Gamleskole utskriftsbasert feilsøking var vanlig før modne feilsøkere ble utviklet. Alternativer til `gdb` inkluderer `lldb`, `dbx`, eller IDE-integrerte feilsøkere som de i Visual Studio eller CLion.

Når du jobber med feilsøkere, varierer implementasjonen—noen kan fange kjøretidsfeil, undersøke minne, eller til og med reversere utførelsen av et program. `gdb` kan feste seg til kjørende prosesser, noe som tillater feilsøking av allerede kjørende programvare, en fordel for å fikse live systemfeil.

## Se Også
- GNU Debugger (GDB): https://www.gnu.org/software/gdb/documentation/
- Feilsøking med GDB: https://sourceware.org/gdb/current/onlinedocs/gdb
- LLDB Debugger: https://lldb.llvm.org/use/tutorial.html
- Feilsøkingsteknikker i C: http://www.cprogramming.com/debugging/debugging.html