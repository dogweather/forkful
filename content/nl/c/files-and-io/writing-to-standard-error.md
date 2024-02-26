---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:15:01.143013-07:00
description: "Schrijven naar de standaardfout in C houdt in dat foutmeldingen en diagnostische\
  \ informatie naar een aparte stroom worden geleid dan de hoofdprogramma-\u2026"
lastmod: '2024-02-25T18:49:48.632575-07:00'
model: gpt-4-0125-preview
summary: "Schrijven naar de standaardfout in C houdt in dat foutmeldingen en diagnostische\
  \ informatie naar een aparte stroom worden geleid dan de hoofdprogramma-\u2026"
title: Schrijven naar standaardfout
---

{{< edit_this_page >}}

## Wat & Waarom?

Schrijven naar de standaardfout in C houdt in dat foutmeldingen en diagnostische informatie naar een aparte stroom worden geleid dan de hoofdprogramma-uitvoer. Programmeurs doen dit om foutmeldingen te scheiden van de standaarduitvoer, waardoor beide makkelijker apart te lezen en te verwerken zijn, vooral bij het debuggen of loggen van de uitvoering van programma's.

## Hoe te:

In C wordt de `stderr`-stroom gebruikt om foutberichten te schrijven. In tegenstelling tot schrijven naar de standaarduitvoer met `printf`, kan het schrijven naar `stderr` gedaan worden met behulp van `fprintf` of `fputs`. Hier is hoe je dat kunt doen:

```c
#include <stdio.h>

int main() {
    fprintf(stderr, "Dit is een foutmelding.\n");

    fputs("Dit is nog een foutmelding.\n", stderr);
    
    return 0;
}
```

Voorbeelduitvoer (naar stderr):
```
Dit is een foutmelding.
Dit is nog een foutmelding.
```

Het is belangrijk om op te merken dat hoewel de uitvoer vergelijkbaar lijkt met `stdout` in de console, het onderscheid duidelijk wordt wanneer omleiding wordt gebruikt in de terminal:

```sh
$ ./uw_programma > output.txt
```

Dit commando leidt alleen de standaarduitvoer om naar `output.txt`, terwijl de foutmeldingen nog steeds op het scherm verschijnen.

## Diepere Duik

Het onderscheid tussen `stdout` en `stderr` in op Unix-gebaseerde systemen gaat terug tot de vroege dagen van C en Unix. Deze scheiding maakt robuustere foutafhandeling en loggen mogelijk, aangezien het programmeurs in staat stelt foutmeldingen onafhankelijk van de standaardprogrammauitvoer om te leiden. Terwijl `stderr` standaard ongebufferd is om onmiddellijke uitvoer van foutmeldingen te garanderen, wat helpt bij het debuggen van crashes en andere kritieke problemen, is `stdout` doorgaans gebufferd, wat betekent dat de uitvoer kan worden vertraagd totdat de buffer wordt geleegd (bijvoorbeeld bij voltooiing van het programma of handmatig legen).

Bij moderne toepassingen is het schrijven naar `stderr` nog steeds relevant, vooral voor opdrachtregelhulpmiddelen en servertoepassingen waarbij het onderscheid tussen reguliere logberichten en fouten cruciaal is. Echter, voor complexere foutafhandeling, vooral in GUI-toepassingen of waar geavanceerdere logmechanismen nodig zijn, kunnen programmeurs gebruik maken van gespecialiseerde logbibliotheken die meer controle bieden over berichtopmaak, bestemmingen (bijvoorbeeld bestanden, netwerk) en ernstniveaus (info, waarschuwing, fout, enz.).

Hoewel `stderr` een fundamenteel mechanisme biedt voor foutrapportage in C, betekent de evolutie van programmeerpraktijken en de beschikbaarheid van geavanceerde logframeworks vaak dat het slechts het startpunt is voor moderne foutafhandelingsstrategieën.
