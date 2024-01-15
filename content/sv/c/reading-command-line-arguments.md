---
title:                "Att läsa kommandoradsargument"
html_title:           "C: Att läsa kommandoradsargument"
simple_title:         "Att läsa kommandoradsargument"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför 
Det är viktigt att kunna läsa kommandoradsargument i C för att kunna skriva program som kan anpassa sig till olika användningsscenarier baserat på användarens input.

## Hur man gör
För att läsa kommandoradsargument i C behöver vi använda två parametrar i main-funktionen: argc och argv. 
Koden nedan visar hur vi kan utskriva varje kommandoradsargument och dess position i argv-arrayen:

```C
int main(int argc, char *argv[]){
    for (int i = 0; i < argc; i++){
        printf("Argument %d: %s\n", i, argv[i]);
    }
    return 0;
}

```
Om vi kör programmet med argumenten "Hej" och "världen" kommer utskriften att se ut såhär:

```
Argument 0: programnamn
Argument 1: Hej
Argument 2: världen
```

Det är värt att notera att kommandoradsargumenten börjar vid index 1, medan argumentet på index 0 alltid är programnamnet självt.

## Djupdykning
Vad händer egentligen bakom kulisserna när vi läser kommandoradsargument i C? 
När vi kör programmet från terminalen tilldelas alla argument som skrivs efter programnamnet till argv-arrayen. 
Därefter räknas antalet argument och lagras i argc-variabeln. 
På så sätt kan vi enkelt iterera genom argv-arrayen och använda argumenten i vårt program. 
Det är också möjligt att använda de inbyggda funktionerna i C för att utföra mer avancerade operationer med kommandoradsargumenten, som att konvertera dem till andra datatyper eller jämföra dem med andra strängar.

## Se även
- [C Programming Tutorial - Command Line Arguments](https://www.tutorialspoint.com/cprogramming/c_command_line_arguments.htm)
- [The argc and argv parameters](https://www.cplusplus.com/articles/DEN36Up4/)
- [Command line arguments in C](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)