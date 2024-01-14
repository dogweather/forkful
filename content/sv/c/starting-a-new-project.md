---
title:                "C: Att börja ett nytt projekt"
simple_title:         "Att börja ett nytt projekt"
programming_language: "C"
category:             "C"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Varför

Att starta ett nytt projekt kan vara en utmanande och spännande upplevelse. Genom programmering kan du skapa något helt nytt och använda din kreativitet och logiska tänkande för att lösa problem. Det är också ett sätt att utveckla och förbättra dina färdigheter som programmerare.

## Så här gör du

För att starta ett nytt C-programmeringsprojekt behöver du först ha en textredigerare installerad på din dator, till exempel Visual Studio Code eller Sublime Text. Därefter behöver du ha en C-kompilator, som GCC, installerad för att kunna köra ditt program.

För att kompilera din kod, öppna din textredigerare och skapa en ny fil med filändelsen `.c`. Börja sedan med att inkludera biblioteket `stdio.h` för att kunna använda standard in- och utmatningsfunktioner. Sedan kan du börja skriva din kod enligt följande:

```C
#include <stdio.h>

int main()
{
    // Kod här
    return 0;
}
```

I `main()`-funktionen kan du sedan skriva din kod och använda olika kontrollstrukturer som `for`-loopar och `if`-satser för att styra programmet. Se nedan för ett enkelt exempel:

```C
#include <stdio.h>

int main()
{
    int num = 5;

    if (num > 0)
    {
        for (int i = 0; i < num; i++)
        {
            printf("Hej från C!\n");
        }
    }

    return 0;
}
```

När du är klar med din kod, spara filen och använd Sedan kommandot `gcc filnamn.c -o programnamn` för att kompilera din kod och skapa en körbar fil. Du kan sedan köra ditt program genom att skriva `./programnamn` i terminalen.

## Djupdykning

Att starta ett nytt projekt innebär inte bara att skriva kod, utan också att planera och organisera ditt arbete. Det är viktigt att ha en tydlig målbild och en strukturerad plan för att undvika kaos i senare skeden av projektet.

En bra början är att skapa en lista över de olika funktioner och delar som ditt program ska innehålla. Detta gör det lättare att identifiera och lösa problem längs vägen. Det kan också vara bra att skriva kommentarer i din kod för att hjälpa dig själv och andra att förstå vad varje del av koden gör.

En annan viktig del av att starta ett nytt projekt är att inse att det är en kontinuerlig process. Bara för att du har avslutat ett projekt betyder det inte att du är klar som programmerare. Genom att ständigt lära dig nya saker och utmana dig själv, blir du bättre och bättre på att skapa högkvalitativa program.

## Se även

För mer information och inspiration om C-programmering, se nedan för några användbara länkar:

- [Learn X in Y minutes](https://learnxinyminutes.com/docs/c/)
- [C Programming Language på TutorialsPoint](https://www.tutorialspoint.com/cprogramming/)
- [Visual Studio Code](https://code.visualstudio.com/)
- [GCC](https://gcc.gnu.org/)