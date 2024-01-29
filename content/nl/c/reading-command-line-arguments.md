---
title:                "Commandoregelargumenten lezen"
date:                  2024-01-28T22:05:12.539425-07:00
model:                 gpt-4-0125-preview
simple_title:         "Commandoregelargumenten lezen"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/reading-command-line-arguments.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Elke programmeur is ongetwijfeld wel eens commandoregelargumenten tegengekomen. Ze laten gebruikers data invoeren in je programma. Ze gebruiken kan drastisch veranderen hoe je programma zich gedraagt zonder de code te wijzigen—denk aan cheatcodes voor gamers, maar dan voor programmeurs.

## Hoe:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("Je hebt %d argumenten ingevoerd:\n", argc);
    for(int i = 0; i < argc; i++) {
        printf("arg%d: %s\n", i, argv[i]);
    }
    return 0;
}
```

Voorbeeld van uitvoer bij aanroepen van `./yourprogram Hello World!`:

```
Je hebt 3 argumenten ingevoerd:
arg0: ./yourprogram
arg1: Hello
arg2: World!
```

## Diepere Duik

Er was eens, in het Unix-tijdperk, toen commandoregelopdrachten de voorkeur hadden. Tegenwoordig zijn GUI's koning, maar commandoregelargumenten zijn verre van uitgestorven—denk aan scripts, geautomatiseerde taken, of complexe applicatieparameters.

Argumenten in C arriveren via twee parameters in `main()`: `argc` (aantal argumenten) en `argv` (argumentvector). `argc` vertelt je hoeveel argumenten er zijn, terwijl `argv` een array van strings is die de daadwerkelijke argumenten bevat, met `argv[0]` als de naam van het programma.

Er zijn alternatieven zoals `getopt()` voor Unix-achtige systemen, die opties en hun argumenten netjes kunnen analyseren. Ook zijn er bibliotheken zoals `argp` die je een handje helpen bij meer complexe analyse scenario's.

De kern van de zaak gaat over het kennen van pointers en arrays, aangezien `argv` een array van tekenwijzer is—essentieel een reeks strings. Wanneer je door de commandoregel heen kauwt, behandelt je programma spaties als scheidingstekens voor argumenten, tenzij aanhalingstekens als je vrienden ziet die een enkel argument omsluiten.

## Zie Ook

- De sectie over [Programma-argumenten](https://www.gnu.org/software/libc/manual/html_node/Program-Arguments.html) in de Handleiding van de GNU C-bibliotheek
- [Wikipedia-pagina over Commandoregelinterface](https://nl.wikipedia.org/wiki/Opdrachtregelinterface#Argumenten)
- ["C Commandoregelargumenten" op tutorialspoint](https://www.tutorialspoint.com/cprogramming/c_command_line_arguments.htm)
