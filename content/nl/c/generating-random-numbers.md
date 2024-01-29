---
title:                "Willekeurige getallen genereren"
date:                  2024-01-28T22:01:04.124285-07:00
model:                 gpt-4-0125-preview
simple_title:         "Willekeurige getallen genereren"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/generating-random-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het genereren van willekeurige getallen in C houdt in dat je reeksen getallen creëert die geen enkel herkenbaar patroon hebben, waarmee het concept van willekeurigheid wordt nagebootst. Programmeurs gebruiken willekeurige getallen voor een veelvoud aan doeleinden, inclusief het simuleren van data, cryptografische toepassingen en spelontwikkeling, wat het een essentieel aspect van programmeren maakt.

## Hoe:

Om willekeurige getallen te genereren in C, gebruik je doorgaans de `rand()` functie die je in `stdlib.h` vindt. Het is echter cruciaal om de generator voor willekeurige getallen te zaaien om variabiliteit in de gegenereerde getallen over verschillende programma-uitvoeringen heen te garanderen. De `srand()` functie, gezaaid met een waarde, vaak de huidige tijd, faciliteert dit.

Hier is een simpel voorbeeld van het genereren van een willekeurig getal tussen 0 en 99:

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // Zaai de generator voor willekeurige getallen
    srand((unsigned) time(NULL));

    // Genereer een willekeurig getal tussen 0 en 99
    int randomNumber = rand() % 100;

    // Print het willekeurige getal
    printf("Willekeurig getal: %d\n", randomNumber);

    return 0;
}
```

Voorbeeld van uitvoer:

```
Willekeurig getal: 42
```

Het is belangrijk om op te merken dat elke uitvoering van dit programma een nieuw willekeurig getal zal produceren, dankzij het zaaien met de huidige tijd.

## Diepgaande Duik

De traditionele manier van het genereren van willekeurige getallen in C, met `rand()` en `srand()`, is niet echt willekeurig. Het is pseudowillekeurig. Dit is prima voor veel toepassingen, maar schiet tekort in situaties die een hoge mate van willekeurigheid vereisen, zoals bij serieuze cryptografische gebruiken. De door `rand()` gegenereerde reeks wordt volledig bepaald door het zaad dat aan `srand()` is gegeven. Dus, als het zaad bekend is, kan de reeks worden voorspeld, waardoor de willekeurigheid vermindert.

Historisch gezien is de `rand()` functie bekritiseerd vanwege zijn lage kwaliteit van willekeurigheid en beperkt bereik. Moderne alternatieven omvatten het gebruik van apparaatspecifieke API's of externe bibliotheken die beter echte willekeur benaderen of, op UNIX-achtige systemen, het lezen van `/dev/random` of `/dev/urandom` voor cryptografische doeleinden.

Bijvoorbeeld, het gebruik van `/dev/urandom` in C:

```c
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *fp;
    unsigned int randomNumber;

    // Open /dev/urandom om te lezen
    fp = fopen("/dev/urandom", "r");

    // Lees een willekeurig getal
    fread(&randomNumber, sizeof(randomNumber), 1, fp);

    // Print het willekeurige getal
    printf("Willekeurig getal: %u\n", randomNumber);

    // Sluit het bestand
    fclose(fp);

    return 0;
}
```

Deze methode leest rechtstreeks uit de entropiepool van het systeem, en biedt een hogere kwaliteit van willekeurigheid die geschikt is voor gevoeliger toepassingen. Deze aanpak kan echter draagbaarheidsproblemen hebben op verschillende platforms, waardoor het minder universeel is dan het gebruik van `rand()`.

Ongeacht de methode, is het begrijpen van de aard van willekeurigheid en de implementatie ervan in C cruciaal voor het ontwikkelen van effectieve, veilige en boeiende toepassingen.
