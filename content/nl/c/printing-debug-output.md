---
title:                "Debug-output afdrukken"
date:                  2024-01-28T22:04:27.472498-07:00
model:                 gpt-4-0125-preview
simple_title:         "Debug-output afdrukken"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/printing-debug-output.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Debuguitvoer afdrukken is als het invoegen van kleine controlepunten in je code om informatie uit te spugen, wat helpt om erachter te komen wat er in godsnaam allemaal gebeurt. Programmeurs doen dit om bugs te lokaliseren of om te zorgen dat hun code stap-voor-stap doet wat het moet doen.

## Hoe:

Hier is het addertje onder het gras—debuguitvoer afdrukken is eenvoudig. De zware last in C is `printf`. Bekijk dit eenvoudige voorbeeld:

```c
#include <stdio.h>

int main() {
    int loopCounter = 0;
    for(loopCounter = 0; loopCounter < 5; loopCounter++) {
        printf("Loop iteratie: %d\n", loopCounter);
        // Meer complexe code hier.
    }
    printf("Loop afgerond.\n");
    // Rest van je code.
    return 0;
}
```

Als je dit uitvoert, verschijnt er op je scherm:

```
Loop iteratie: 0
Loop iteratie: 1
Loop iteratie: 2
Loop iteratie: 3
Loop iteratie: 4
Loop afgerond.
```

Simpel, toch? Vergeet alleen niet om deze regels te verwijderen of uit te commentariëren als je klaar bent, zodat je console niet vol raakt.

## Diepe Duik

Vroeger waren er geen fancy Integrated Development Environment (IDE) debuggers om je bij de hand te nemen. Ruwe output naar de terminal was alles wat je had. Vandaag de dag is het nog steeds goud waard voor snelle en vuile diagnostiek.

Alternatieven? Nou, voor zwaar debuggen, kun je overschakelen naar het gebruik van echte IDE debuggers, of logfaciliteiten die meer controle bieden.

`printf` is je go-to, maar er is meer onder de motorkap. Bijvoorbeeld, `fprintf(stderr, ...)` kan je berichten omleiden naar de standaardfoutstroom, waardoor ze makkelijker te scheiden zijn van standaarduitvoer.

Ook, als prestaties belangrijk zijn, wil je misschien loggen in strakke lussen vermijden of overwegen om te compileren met macro's die je in staat stellen om debugcode in productie uit te strippen.

## Zie Ook

- [GNU Debugger (GDB)](https://www.gnu.org/software/gdb/) voor wanneer je klaar bent om verder te gaan dan `printf`.
- [C Logging Bibliotheken](https://www.slant.co/topics/1183/~best-logging-add-ons-for-c-programming) voor gestructureerd loggen.
- [Learn C The Hard Way](https://learncodethehardway.org/c/) voor een diepere duik in de bredere wereld van C-programmering.
