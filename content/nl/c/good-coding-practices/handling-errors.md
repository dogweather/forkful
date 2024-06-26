---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:09.804056-07:00
description: "Hoe te: C heeft geen ingebouwde ondersteuning voor uitzonderingen zoals\
  \ sommige andere talen. In plaats daarvan vertrouwt het op enkele conventionele\u2026"
lastmod: '2024-03-13T22:44:51.298766-06:00'
model: gpt-4-0125-preview
summary: C heeft geen ingebouwde ondersteuning voor uitzonderingen zoals sommige andere
  talen.
title: Fouten afhandelen
weight: 16
---

## Hoe te:
C heeft geen ingebouwde ondersteuning voor uitzonderingen zoals sommige andere talen. In plaats daarvan vertrouwt het op enkele conventionele foutafhandelingsstrategieën, zoals het teruggeven van speciale waarden uit functies en het instellen van globale variabelen zoals `errno`.

**Speciale Waarden Teruggeven**

Functies kunnen fouten aangeven door een specifieke waarde terug te geven die waarschijnlijk geen geldig resultaat is. Hier is een voorbeeld met gehele getallen:

```c
#include <stdio.h>

int inverse(int number, double *result) {
    if (number == 0) {
        return -1; // Foutgeval
    } else {
        *result = 1.0 / number;
        return 0; // Succes
    }
}

int main() {
    double result;
    if (inverse(0, &result) < 0) {
        printf("Fout: Delen door nul.\n");
    } else {
        printf("De inverse is: %f\n", result);
    }
    
    return 0;
}
```

**Uitvoer:**
```
Fout: Delen door nul.
```

**`errno` Controleren**

Voor bibliotheekfuncties, vooral diegene die interageren met het systeem of OS (zoals bestands-I/O), wordt `errno` ingesteld wanneer een fout optreedt. Om het te gebruiken, moet je `errno.h` includen en `errno` controleren na een vermoedelijke mislukking:

```c
#include <stdio.h>
#include <errno.h>
#include <string.h>

int main() {
    FILE *file = fopen("nonexistent.txt", "r");
    if (file == NULL) {
        printf("Fout bij het openen van bestand: %s\n", strerror(errno));
    }
    
    return 0;
}
```

**Uitvoer:**
```
Fout bij het openen van bestand: Bestand of map bestaat niet
```

## Diepgaande Verkenning
Historisch gezien heeft het minimalistische ontwerp van de C-programmeertaal uitgesloten dat er een ingebouwd mechanisme voor uitzonderingsafhandeling is, wat een weerspiegeling is van zijn oorsprong in systeemprogrammering waar maximale prestatie en controle dichtbij de machine cruciaal zijn. In plaats daarvan adopteert C een meer handmatige benadering van foutafhandeling die past bij zijn filosofie van het geven van zoveel mogelijk controle aan programmeurs, zelfs ten koste van het gemak.

Hoewel deze aanpak goed aansluit bij de ontwerpdoelen van C, kan het ook leiden tot uitgebreide foutcontrolecode en de mogelijkheid tot gemiste foutcontroles, die moderne talen aanpakken met gestructureerde uitzonderingsafhandelingsmechanismen. Uitzonderingen in talen zoals Java of C# maken centrale foutverwerking mogelijk, waardoor code schoner wordt en foutbeheer eenvoudiger. Echter, uitzonderingen brengen hun eigen overhead en complexiteit met zich mee, wat mogelijk niet ideaal is voor systeemniveau-programmering waar C uitblinkt.

Ondanks zijn grofheid heeft deze handmatige foutafhandeling in C het ontwerp van foutbeheer in veel andere talen geïnformeerd, waardoor een model ontstaat waar de explicietheid van foutcondities kan leiden tot voorspelbaardere en beter te debuggen code. Voor kritieke systemen, waar storingen sierlijk moeten worden beheerd, zorgt C's foutafhandelingsparadigma - gecombineerd met moderne beste praktijken zoals foutafhandelingsbibliotheken en conventies - voor robuustheid en betrouwbaarheid.
