---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:06:10.851635-07:00
description: "Hur man g\xF6r: I C kan `main`-funktionen utformas f\xF6r att acceptera\
  \ kommandoradsargument genom att anv\xE4nda parametrarna `int argc` och `char *argv[]`.\
  \ H\xE4r\u2026"
lastmod: '2024-03-13T22:44:38.400020-06:00'
model: gpt-4-0125-preview
summary: "I C kan `main`-funktionen utformas f\xF6r att acceptera kommandoradsargument\
  \ genom att anv\xE4nda parametrarna `int argc` och `char *argv[]`."
title: "L\xE4ser in kommandoradsargument"
weight: 23
---

## Hur man gör:
I C kan `main`-funktionen utformas för att acceptera kommandoradsargument genom att använda parametrarna `int argc` och `char *argv[]`. Här representerar `argc` antalet argument som passerats, och `argv` är en array av teckenpekare som listar alla argument. Här är ett snabbt exempel för att illustrera:

```c
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("Programnamn: %s\n", argv[0]);
    printf("Antal Argument: %d\n", argc - 1);
    for (int i = 1; i < argc; i++) {
        printf("Argument %d: %s\n", i, argv[i]);
    }
    return 0;
}
```

Genom att använda ovanstående kod, om programmet körs som `./programnamn -a exempel`, skulle utmatningen bli:

```
Programnamn: ./programnamn
Antal Argument: 2
Argument 1: -a
Argument 2: exempel
```

Detta demonstrerar hur kommandoradsargument kan tolkas och användas i ett C-program.

## Fördjupning
Konventionen att skicka argument till program går tillbaka till de tidigaste dagarna av Unix. I detta traditionella tillvägagångssätt ger `argc` och `argv` ett enkelt men kraftfullt gränssnitt för kommandoradsinteraktion, vilket personifierar Unix filosofi av små, modulära verktyg som arbetar tillsammans. Även om moderna språk ofta introducerar mer sofistikerade bibliotek eller ramverk för att tolka kommandoradsargument, erbjuder direktmetoden i C oöverträffad transparens och kontroll.

I nyliga utvecklingar har bibliotek såsom `getopt` i POSIX-system utvecklats för att stödja mer komplexa tolkningsbehov, som att hantera långa alternativnamn eller standardvärden för saknade argument. Ändå kvarstår den grundläggande mekanismen för `argc` och `argv` som väsentlig för att förstå hur program interagerar med sin körtidsmiljö i C.

Kritiker kan argumentera att hantering av `argc` och `argv` direkt kan vara felbenägen, och framhåller användningen av högre nivås abstraktioner. Ändå, för de som strävar efter att behärska C:s komplexiteter och uppskatta nyanserna i dess lågnivåoperationer, är mästring av kommandoradsargument en rättighet. Denna blandning av historisk metodik och praktiskt nytta förkroppsligar mycket av C:s bestående attraktion i systemprogrammering och mjukvaruutveckling.
