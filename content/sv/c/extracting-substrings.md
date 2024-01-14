---
title:                "C: Extrahering av delsträngar"
simple_title:         "Extrahering av delsträngar"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Ibland när du arbetar med textsträngar kan du behöva delar av strängen istället för hela. Det är här som utvinning av substrängar kommer till nytta! Genom att utvinna substrängar kan du enkelt plocka ut den information du behöver utan att behöva bearbeta hela strängen.

## Hur man gör det

För att utvinna en substräng i C använder du funktionerna `strncpy()` och `strncat()`. `strncpy()` kopierar de tecken du vill ha från en sträng till en annan och `strncat()` lägger till tecknen på slutet av en befintlig sträng. Här är ett exempel på hur du kan använda dessa funktioner för att utvinna en substräng:

```C
#include <stdio.h>
#include <string.h>

int main(){

    char str1[20] = "Hej alla vänner";
    char str2[20];

    // Kopierar de första 3 tecknen av str1 till str2
    strncpy(str2, str1, 3);

    // Lägger till ett null-tecken för att avsluta strängen
    str2[3] = '\0';

    printf("Den utvinnade substrängen är: %s", str2);
    
    return 0;
}
```

Output:
```
Den utvinnade substrängen är: Hej
```

Det finns också andra sätt att utvinna substrängar beroende på dina specifika behov. Du kan till exempel använda funktionen `strstr()` för att hitta en viss sträng inuti en större sträng och sedan använda funktionerna `strncpy()` och `strncat()` för att kopiera eller lägga till tecken. Det finns många olika kombinationer och sätt att arbeta med utvinning av substrängar i C, så det är viktigt att experimentera och hitta den bästa metoden för ditt specifika användningsområde.

## Djupdykning

Utvinning av substrängar är en viktig del av C-programmering eftersom det ger dig möjlighet att hantera textsträngar på ett effektivt sätt. Genom att använda de inbyggda funktionerna som nämndes ovan, kan du undvika att behöva skapa komplicerad kod för att hantera tecken och strängar.

En annan viktig aspekt att tänka på när du arbetar med utvinning av substrängar är hantering av minnet. Eftersom substrängar är delar av en större sträng, måste du säkerställa att du allokerar tillräckligt med minne för den utvinnade substrängen för att undvika att få problem med minne i ditt program.

## Se också

Här är några användbara resurser för att lära dig mer om utvinning av substrängar i C:

- [Officiell dokumentation för CString-biblioteket](https://www.cplusplus.com/reference/cstring/)
- [Tutorial för utvinning av substrängar i C](https://www.idreamincode.com/blog/substring-in-c/)
- [Strängmanipulering i C](https://www.studytonight.com/c/string-manipulation)