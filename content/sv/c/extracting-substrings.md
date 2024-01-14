---
title:    "C: Extrahera substrängar"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c/extracting-substrings.md"
---

{{< edit_this_page >}}

# Varför använda substrängar i C

Att extrahera substrängar är en viktig del av programmering på C-språket. Det är en nödvändig färdighet för att hantera textsträngar och används ofta i utvecklingen av appar, spel och andra program som arbetar med data.

## Så här extraherar du substrängar i C

Det finns flera olika sätt att extrahera substrängar i C, men det vanligaste sättet är att använda funktionen "strncpy", vilken kopierar en viss del av en textsträng till en annan variabel.

```C
#include <stdio.h>
#include <string.h>
 
int main () {
   char str1[20] = "Hej, världen!";
   char str2[10];
 
   /* Kopiera de första 5 tecknen av strängen str1 till str2 */
   strncpy(str2, str1, 5);
 
   printf("Sträng 1 : %s\n", str1);
   printf("Sträng 2 : %s\n", str2);
 
   return 0;
}
```

**Output:**

```bash
Sträng 1 : Hej, världen!
Sträng 2 : Hej, 
```

Det finns också andra funktioner som kan användas för att extrahera substrängar, såsom "strchr" och "strstr", beroende på vilken del av strängen du behöver extrahera.

## Djupdykning i substrängar

Att extrahera substrängar kan verka som en enkel uppgift, men det finns flera saker att tänka på för att undvika buggar och ineffektiv kod. Några viktiga punkter att tänka på inkluderar:

- Att alltid se till att slutet av den extraherade substrängen är korrekt terminerad med en nolltecken (vilket markerar slutet av en sträng i C).
- Att beakta storleken på den extraherade substrängen för att undvika bufferöverflöden.
- Att vara medveten om att den ursprungliga strängen kan ändras efter att en substräng har extraherats, särskilt om man använder "strncpy".

En annan viktig aspekt av substrängsextraktion är prestanda. I vissa fall kan det vara mer effektivt att manipulera en hel sträng istället för att extrahera en del av den. Det är också viktigt att undvika onödiga iterationer och använda lämpliga funktioner för att undvika en långsam eller ineffektiv kod.

## Se även

- [C-programmering](https://sv.wikipedia.org/wiki/C_%28programspr%C3%A5k%29)
- [Strängmanipulation i C](https://www.cprogramming.com/tutorial/c/lesson9.html)
- [Programmeringsspråket C - Grundläggande element](https://www.inf.unibz.it/~ricci/website/uploads/Teaching/A-02-1B5LAN-programmeringsspraket-c.pdf)