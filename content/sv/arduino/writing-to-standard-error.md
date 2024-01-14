---
title:    "Arduino: Skrivning till standardfel"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Varför
Att skriva kod till standardfel är en viktig del av felets hantering i ditt Arduino-program. Genom att skriva till standardfel kan du fånga och hantera felmeddelanden som uppstår under exekvering av din kod. Detta gör det möjligt för dig att felsöka och lösa problem i ditt program på ett mer effektivt sätt.

## Så här gör du
Det första steget är att inkludera biblioteket "cstdio" i din kod. Detta ger dig tillgång till funktionen "fprintf", som låter dig skriva till standardfel. Här är en enkel kodsnutt för att skriva ett felmeddelande till standardfel:

```Arduino
#include <cstdio>

int main(){
    fprintf(stderr, "Ett fel uppstod i programmet.");
    return 0;
}
```

När du kör ditt program kommer du se följande utskrift till standardfel:

```
Ett fel uppstod i programmet.
```

Du kan också använda formatsträngar och variabler för att skriva mer detaljerade felmeddelanden, precis som du skulle göra med "printf" för att skriva till standardutgång. Här är ett exempel på hur du kan skriva ett felmeddelande som innehåller en variabel till standardfel:

```Arduino
#include <cstdio>

int main(){
    int num = 2;
    fprintf(stderr, "Ett fel uppstod vid hantering av numret %d.", num);
    return 0;
}
```

Output:

```
Ett fel uppstod vid hantering av numret 2.
```

## Djupdykning
När ditt program körs, är standardutgången vanligtvis ansluten till en display eller serieport, medan standardfel är ansluten till en debug-port eller spårningsport. Genom att skriva till standardfel kan du separera utdata som är avsedd för felsökning och spårning från den vanliga utdatan.

Det är också bra att veta att standardfel vanligtvis riktar sig till avancerade användare och utvecklare som har tillgång till dessa debug- och spårningsportar. Om du vill inkludera en felhanteringsmekanism för en mer allmän användare, kan det vara bättre att använda funktionen "Serial.print", som låter dig skriva till serieporten.

## Se även
- [Arduino - Standard error](https://www.arduino.cc/reference/en/language/functions/communication/standarderror/)
- [GNU C Library - fprintf](https://www.gnu.org/software/libc/manual/html_node/Error-Reporting.html#Error-Reporting)