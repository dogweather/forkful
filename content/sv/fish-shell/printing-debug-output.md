---
title:                "Utskrift av felsökningsutdata"
html_title:           "Fish Shell: Utskrift av felsökningsutdata"
simple_title:         "Utskrift av felsökningsutdata"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva ut debuggutdata är ett sätt för programmerare att kontrollera vad som händer i sin kod. Det kan också hjälpa till att hitta och lösa fel i programmet.

## Hur man gör:

### Skriv ut till terminalen

```Fish Shell
echo "Debugging output is here!" 
```
Output:
```
Debugging output is here!
```

### Visa variabler

```Fish Shell
set my_variable "This is a variable"
echo $my_variable
```
Output:
```
This is a variable
```

### Använd en debuggande funktion

Fish Shell har en inbyggd funktion för att skriva ut debuggutdata i formatet "[DEBUG]: <din utdata>". Detta kan vara ett bra sätt att separera din debuggutdata från andra utskrifter i ditt program.

```Fish Shell
debug "This is my debugging output"
```
Output:
```
[DEBUG]: This is my debugging output
```

## Deep Dive:

### Historiskt sammanhang

Att skriva ut debuggutdata är ett vanligt sätt att kontrollera och felsöka kod i många programmeringsspråk. Det har funnits med sedan de tidigaste dagarna av programmering och är fortfarande en viktig del av utvecklingen idag.

### Alternativ till utskrift i terminalen

Förutom att skriva ut till terminalen finns det andra sätt att visa debuggutdata, till exempel att skriva till en loggfil som kan granskas senare. Det finns också olika verktyg och bibliotek som kan hjälpa dig att granska och analysera din debuggutdata.

### Implementeringsdetaljer

I Fish Shell kan du använda enkla kommandon som `echo` eller `set` för att visa debuggutdata. Du kan också använda funktionen `debug` för att skriva ut i ett mer standardiserat format. Se `man fish` för mer information om varje kommando och dess användningsområden.

## Se även:

- [Fish Shell dokumentation](https://fishshell.com/docs/current/index.html)
- [The art of debugging](https://hbr.org/2018/02/the-art-of-debugging) (engelska)