---
title:                "Fish Shell: Generera slumpmässiga tal"
programming_language: "Fish Shell"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga nummer kan vara användbart för många olika applikationer, såsom spel, kryptering eller testning. Med Fish Shell finns det flera olika sätt att skapa dessa nummer, vilket gör det till ett användbart verktyg för programmerare.

## Så här gör du

Generera slumpmässiga nummer med Fish Shell är enkelt. Du kan använda inbyggda kommandon som `random`, som genererar ett slumpmässigt decimaltal mellan 0 och 1. Du kan också använda `seq` för att generera en sekvens av slumpmässiga heltal.

```Fish Shell
# Generera ett slumpmässigt decimaltal
random

# Generera en sekvens av 10 slumpmässiga heltal mellan 1 och 100
seq 10 | random -n -r 1 100
```

Det finns också möjlighet att ställa in en seed, vilket säkerställer att samma sekvens av slumpmässiga nummer genereras varje gång. Detta kan vara användbart för testning och debugging. Använd kommandot `set -U` för att ställa in en seed.

## Djupdykning

För att förstå hur Fish Shell genererar slumpmässiga tal, måste vi titta på dess underliggande implementering. Fish Shell använder en pseudorandom nummergenerator baserad på Multiplicative Linear Congruential Generator (MLCG) algoritmen. Denna algoritm använder en grundläggande formel för att återanvända ett tidigare tal för att generera nästa tal i sekvensen. Detta kan skapa en sekvens av tal som är "slumpmässiga" men som kan förutses om seed-numret är känt.

## Se också

- [Fish Shell dokumentation om att generera slumpmässiga tal] (https://fishshell.com/docs/current/cmds/random.html)
- [Implementering av Multiplicative Linear Congruential Generator] (https://en.wikipedia.org/wiki/Linear_congruential_generator)
- [Slumpmässighetens filosofi och kritik] (https://plato.stanford.edu/entries/randomness/)