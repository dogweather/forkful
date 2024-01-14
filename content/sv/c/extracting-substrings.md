---
title:                "C: Extrahering av delsträngar"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför
I denna bloggpost kommer vi att gå igenom hur man extraherar substrängar i C-programmering. Detta kan vara användbart när du behöver arbeta med strängar och bara behöver en del av dem. Att kunna extrahera substrängar ger dig större flexibilitet och effektivitet i ditt program.

## Hur man gör det
För att extrahera en substräng i C, måste du använda funktionen "strncpy" från standardbiblioteket "string.h". Det tar tre parametrar - målsträngen, källsträngen och antalet tecken som ska kopieras. Här är ett enkelt exempel som extraherar de första sex tecknen från en sträng:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str1[] = "Jag älskar programmering!";
    char str2[7];

    // Kopiera de första 6 tecknen från str1 till str2
    strncpy(str2, str1, 6);

    // Avsluta str2 med en null-tecken
    str2[6] = '\0';

    // Skriv ut resultatet
    printf("Den extraherade substrängen är: %s\n", str2);

    return 0;
}
```

Kodblocket ovan skulle ge följande resultat:

```
Den extraherade substrängen är: Jag äl
```

Du kan också använda funktionen "strstr" för att hitta en delsträng inom en större sträng och sedan använda "strncpy" för att extrahera den delsträngen. Här är ett exempel på hur du kan använda detta:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str1[] = "Programmering är kul!";
    char str2[] = "kul";
    char *ptr;

    // Hitta "kul" i str1
    ptr = strstr(str1, str2);

    // Kopiera allt efter "kul" till en ny sträng
    char str3[4];
    strncpy(str3, ptr + strlen(str2), 3);

    // Avsluta str3 med en null-tecken
    str3[3] = '\0';

    // Skriv ut resultatet
    printf("Den extraherade substrängen är: %s\n", str3);

    return 0;
}
```

Kodblocket ovan kommer att ge följande resultat:

```
Den extraherade substrängen är: !
```

## Djupdykning
Nu när du har en grundläggande förståelse för hur man extraherar substrängar i C, låt oss titta på några viktiga saker att tänka på:

- När du använder "strncpy" för att extrahera en del av en sträng, måste du se till att målsträngen (i vårt första exempel "str2") är tillräckligt stor för att rymma den extraherade substrängen. Annars kan du få överlappande tecken eller till och med ett programkrasch.
- Om du använder "strncpy" för att kopiera delar av en sträng till en ny sträng, se till att avsluta den nya strängen med ett null-tecken för att undvika oönskade tecken.
- Var noga med att hålla ordning på index och antal tecken när du använder "strncpy". Om du till exempel vill kopiera de sista tre tecknen från en sträng måste du använda "strlen" för att beräkna positionen för substrängen.

## Se även
- [C-programmering på Codecademy (på svenska)](https://www.codecademy.com/learn/learn-c)
- [Officiell dokumentation för "string.h"](https://www.tutorialspoint.com/c_standard_library/string_h.htm)