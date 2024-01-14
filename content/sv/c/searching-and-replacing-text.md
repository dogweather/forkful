---
title:                "C: Sökning och ersättning av text"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Att söka och ersätta text är en viktig del av programmering, särskilt när man arbetar med stora mängder textbaserad data. Genom att effektivt kunna söka och ersätta text kan du spara tid och undvika manuella fel när du ändrar eller uppdaterar kod.

## Hur man gör

I C-programmeringsspråket finns det olika sätt att söka och ersätta text. Ett av de vanligaste sätten är att använda funktionen `strchr()` som söker igenom en sträng efter ett specifikt tecken och returnerar en pekare till den första förekomsten av tecknet. Ett exempel på detta skulle kunna se ut så här:

```C
char str[50] = "Välkommen till min blogg";
char *ptr;

ptr = strchr(str, 'l'); // söker efter första förekomsten av bokstaven 'l'
printf("Pekaren pekar på '%s' \n", ptr); // utskrift: "Pekaren pekar på 'llkommen till min blogg'"
```

Alternativt kan du använda funktionen `strstr()` som söker igenom en sträng efter en specifik delsträng och returnerar en pekare till den första förekomsten av delsträngen. Exempelvis:

```C
char str[50] = "Välkommen till min blogg";
char *ptr;

ptr = strstr(str, "min"); // söker efter den första förekomsten av delsträngen "min"
printf("Pekaren pekar på '%s' \n", ptr); // utskrift: "Pekaren pekar på 'min blogg'"
```

Det finns även möjlighet att använda sig av reguljära uttryck för mer avancerad sökning och ersättning av text. Detta kan utföras med hjälp av biblioteket `<regex.h>` och dess funktioner som t.ex. `regcomp()` och `regexec()`. Exempel på detta kan se ut så här:

```C
#include <stdio.h>
#include <regex.h>

int main()
{
    // regcomp() används för att kompilera ett reguljärt uttryck
    // i detta fall söker vi efter siffror i en sträng
    regex_t regex;
    regcomp(&regex, "[0-9]", 0);

    // regexec() utför sökning och returnerar antalet träffar
    // i detta fall söker vi i strängen "Jag är 25 år gammal"
    int match = regexec(&regex, "Jag är 25 år gammal", 0, NULL, 0); 
    printf("Antal träffar: %d \n", match); // utskrift: "Antal träffar: 1"

    return 0; 
}
```

## Djupdykning

Att kunna effektivt söka och ersätta text är en viktig del av programmering men det kräver även kunskap om olika sökalgoritmer och datastrukturer för att kunna utföra det på ett effektivt sätt. Vid stora textfiler kan t.ex. en binärsökalgoritm vara mer effektiv än en linjär sökalgoritm.

Det är även viktigt att ha kunskap om vilka begränsningar som finns för sökning och ersättning av text i olika programmeringsspråk, då vissa språk endast tillåter att söka efter en eller flera tecken istället för en hel delsträng. Det är också viktigt att tänka på prestandaskillnader när man väljer mellan olika metoder och funktioner för att söka och ersätta text.

## Se även

- [C - Strängmanipulering](https://www.programiz.com/c-programming/c-strings)
- [C - reguljära uttryck med regex.h](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)
- [Effektiva sökalgoritmer](https://www.geeksforgeeks.org/searching-algorithms/)