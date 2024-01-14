---
title:    "C: Skapa slumpmässiga nummer"
keywords: ["C"]
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga nummer kan vara mycket användbart inom programmering. Oavsett om du behöver skapa ett spel med en slumpmässig karaktär eller ett verifieringsnummer för dina användare, kan slumpmässiga nummer både göra din kod mer intressant och mer säker.

## Hur du gör det

I C-programmering, kan du generera slumpmässiga nummer med hjälp av `rand()` funktionen. Men innan du använder denna funktion, måste du först inkludera `<stdlib.h>` biblioteket och använda funktionen `srand()` för att "så fröet" för slumpmässigheten. Här är ett exempel på hur du kan generera 10 slumpmässiga tal mellan 1 och 10:

```C
#include <stdio.h>
#include <stdlib.h>

int main()
{
  int i;
  srand(time(0)); // "Så fröet" för slumpmässigheten baserat på aktuell tid
  for(i = 0; i < 10; i++)
  {
    printf("%d\n", rand() % 10 + 1); // Slumpmässigt tal mellan 1 och 10
  }

  return 0;
}
```

Det finns många olika sätt att använda `rand()` funktionen för att skapa olika typer av slumpmässiga resultat. Till exempel, om du vill generera decimaltal istället för heltal, kan du använda `double` datatypen tillsammans med `rand()` funktionen. Det är viktigt att komma ihåg att slumpmässiga nummer inte är helt slumpmässiga, utan baserade på ett förprogrammerat "frö". Du kan också använda funktioner som `time()` för att säkerställa att fröet förändras varje gång programmet körs, vilket ger mer slumpmässiga resultat.

## Djupdykning

Slumpmässiga nummer är en viktig del av den moderna programmeringsvärlden. De används inte bara för spel och användarverifiering, utan även inom algoritmer som behöver vara robusta och oförutsägbara. Tekniker som "pseudo-random number generators" (PRNGs) har utvecklats för att förbättra kvaliteten och slumpmässigheten hos genererade nummer. Dessa PRNGs är matematiska formler som räknar ut nästa nummer i en sekvens baserat på det föregående numret. De ger en högre grad av slumpmässighet jämfört med den enklare `rand()` funktionen.

## Se även

- [C: Att skapa slumpmässiga tal](https://www.sis.uta.fi/~csjhu/cs4610/notes/Random-Numbers.pdf)
- [Slumpmässiga nummer på C-programmeringsspråket](https://overiq.com/c-programming-101/the-random-number-functions-in-c/)
- [Begreppet "så fröet" på C-programspråk](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)