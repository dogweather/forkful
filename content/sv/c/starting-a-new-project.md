---
title:                "Att börja ett nytt projekt"
html_title:           "C: Att börja ett nytt projekt"
simple_title:         "Att börja ett nytt projekt"
programming_language: "C"
category:             "C"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att starta ett nytt projekt innebär att du börjar skriva en ny programkod från grunden. Det kan vara något enkelt som ett kalkylblad eller en komplex applikation. Programerare startar ofta nya projekt för att lösa specifika problem eller för att bygga något nytt och användbart.

## Så här:
Ett enkelt sätt att starta ett nytt projekt är att använda kommandot ```C gcc nyttprojekt.c -o nyttprojekt ``` för att kompilera din kod och skapa ett körbart program. Här är ett exempel på en enkel "Hello World"-applikation:

```C
#include <stdio.h>

int main() {
    // Printar ut "Hello, world!" på konsolen
    printf("Hello, world!");

    return 0;
}
```

Output:
```
Hello, world!
```

## Djupare dykning:
I det förflutna var det vanligt att programmerare började med en tom fil och skrev kod manuellt. Nu finns det dock många verktyg som kan hjälpa till att starta nya projekt på ett enklare sätt, som till exempel integrerade utvecklingsmiljöer (IDE:er) och projektgeneratorer. Alternativt kan du också använda dig av ett programmeringsspråk som är mer anpassat för din specifika applikation, som till exempel Python för skriptning eller Java för stora projekt med många utvecklare. När det kommer till själva implementationen av ditt projekt finns det många olika metoder och designmönster som kan användas.

## Se även:
- [Olika verktyg för att starta nya projekt](https://dev.to/dave/start-your-next-project-with-these-time-saving-tools-3142)
- [Introduktion till designmönster](https://www.tutorialspoint.com/design_pattern/design_pattern_overview.htm)