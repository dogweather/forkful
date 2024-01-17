---
title:                "Använda reguljära uttryck"
html_title:           "C: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Vad & Varför?
Reguljära uttryck, eller regex, är ett vanligt verktyg som används inom programmering för att söka och manipulera textsträngar. Det är ett uttrycksspråk som gör det möjligt att beskriva mönster i texten på ett flexibelt sätt. Programmers använder regex för att effektivt hitta, ersätta eller validera text i en mängd olika program och applikationer.

# Hur man:
För att använda reguljära uttryck i C, behöver vi inkludera biblioteket `regex.h` och sedan skapa ett regex-objekt med hjälp av funktionen `regcomp()`. Sedan kan vi använda detta objekt för att söka och manipulera text med hjälp av olika funktioner som `regexec()` och `regsub()`. Här är ett exempel på hur man söker efter ett ord i en textsträng:

```C
#include <stdio.h>
#include <regex.h>

int main(void)
{
    char *str = "Hello world!";
    regex_t regex;
    int result = regcomp(&regex, "hello", 0); // Skapa regex-objektet "regex" med sökningen "hello"
    result = regexec(&regex, str, 0, NULL, 0); // Söker efter "hello" i strängen "Hello world!"
    if (result == 0) // Om sökningen lyckas, kommer result vara 0
        printf("Sökningen matchade!");
    else if (result == REG_NOMATCH) // Om sökningen inte hittar någon matchning, kommer result vara REG_NOMATCH
        printf("Ingen matchning hittades.");
    else // Om det uppstår ett fel, kommer result vara något annat än 0 eller REG_NOMATCH
        printf("Fel uppstod.");
    
    regfree(&regex); // Frigör minnet som används av regex-objektet
    return 0;
}
```
Output: `Sökningen matchade!`

# Djupdykning:
Reguljära uttryck utvecklades redan på 1950-talet inom fältet formell språkteori. Sedan dess har de blivit en viktig del av många programmeringsspråk och verktyg. Andra sätt att söka och manipulera text inkluderar strstr() -funktionen i C och grep-kommandot i Unix-operativsystem.

Reguljära uttryck erbjuder en kompakt och kraftfull lösning för att hantera textmanipulering, men de kan också vara utmanande att lära sig. Det finns många olika symboler och speciella tecken som kan användas för att beskriva mönster, och felaktiga uttryck kan leda till oväntade resultat. Det är viktigt att öva och testa uttryck noggrant för att få önskat resultat.

# Se även:
- [Regexp Cheatsheet för C](https://www.rexegg.com/regex-c.html)
- [regexp(3) manual page](https://www.unix.com/man-page/osx/3/regexp/)
- [Regular Expressions i C - En enkel handledning](https://www.youtube.com/watch?v=Qw4zFSQHk5o)