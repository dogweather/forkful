---
title:                "Att starta ett nytt projekt"
html_title:           "Arduino: Att starta ett nytt projekt"
simple_title:         "Att starta ett nytt projekt"
programming_language: "C"
category:             "C"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Starta ett nytt projekt i C

## Vad & Varför?

Att starta ett nytt projekt innebär att skapa en grund för programkod och infrastruktur. Programmerare initierar nya projekt för att lösa specifika problem, exploatera möjligheter eller förbättra befintliga system.

## Hur man:

Ett nytt C-projekt kan inledas med att skapa en enkel .c fil. Vi tar HelloWorld-programmet som ett exempel.

```C
#include <stdio.h>

int main() {
   // printf() displays the string inside quotation
   printf("Hello, World!");
   return 0;
}
```

När vi kör detta program kommer det att visa:

```
Hello, World!
```

## Fördjupning

Historiskt sett, blev C utvecklat 1972 av Dennis M. Ritchie på Bell Laboratories. Det används fortfarande flitigt, och är grunden för andra programmeringsspråk som C++, C#, och Objective-C.

Alternativen till C inkluderar språk som Python, Java, och Ruby. Dessa språk kan vara mer lämpliga för vissa projekt, beroende på kraven.

När du startar ett nytt projekt i C, finns det tre huvudkomponenter att tänka på: kodbasen, buildsystemet och dokumentationen. Kodbasen inkluderar alla .c och .h filer. Buildsystemet, till exempel Makefile, bestämmer hur ditt projekt kompileras till ett körbart program. Slutligen, dokumentationen hjälper andra utvecklare att förstå och bidra till ditt projekt.

## Se också:

1. För mer om C programmering: [C Tutorial](https://www.learn-c.org/)
2. En guide för att skapa en Makefile: [Makefile Tutorial](https://makefiletutorial.com/)