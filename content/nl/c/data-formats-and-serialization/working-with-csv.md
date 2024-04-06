---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:54.473087-07:00
description: "Hoe: Om een CSV-bestand in C te lezen, gebruiken we standaardbestands-I/O-functies\
  \ samen met tekenreeksmanipulatiefuncties om elke regel te ontleden.\u2026"
lastmod: '2024-03-13T22:44:51.314394-06:00'
model: gpt-4-0125-preview
summary: Om een CSV-bestand in C te lezen, gebruiken we standaardbestands-I/O-functies
  samen met tekenreeksmanipulatiefuncties om elke regel te ontleden.
title: Werken met CSV
weight: 37
---

## Hoe:


### CSV-bestanden lezen
Om een CSV-bestand in C te lezen, gebruiken we standaardbestands-I/O-functies samen met tekenreeksmanipulatiefuncties om elke regel te ontleden. Hieronder staat een basisvoorbeeld van het lezen van een CSV-bestand en het afdrukken van de velden van elke rij naar de console.

```c
#include <stdio.h>
#include <string.h>

int main() {
    FILE *fp = fopen("data.csv", "r");
    if (!fp) {
        printf("Kan bestand niet openen\n");
        return 1;
    }

    char buf[1024];
    while (fgets(buf, 1024, fp)) {
        char *veld = strtok(buf, ",");
        while(veld) {
            printf("%s\n", veld);
            veld = strtok(NULL, ",");
        }
    }

    fclose(fp);
    return 0;
}
```
Voorbeeld `data.csv`:
```
Naam,Leeftijd,Beroep
John Doe,29,Software Ingenieur
```

Voorbeelduitvoer:
```
Naam
Leeftijd
Beroep
John Doe
29
Software Ingenieur
```

### Schrijven naar CSV-bestanden
Op dezelfde manier involves schrijven naar een CSV-bestand het gebruik van `fprintf` om gegevens op te slaan in een door komma's gescheiden formaat.

```c
#include <stdio.h>

int main() {
    FILE *fp = fopen("output.csv", "w");
    if (!fp) {
        printf("Kan bestand niet openen\n");
        return 1;
    }

    char *koppen[] = {"Naam", "Leeftijd", "Beroep", NULL};
    for (int i = 0; koppen[i] != NULL; i++) {
        fprintf(fp, "%s%s", koppen[i], (koppen[i + 1] != NULL) ? "," : "\n");
    }
    fprintf(fp, "%s,%d,%s\n", "Jane Doe", 27, "Datawetenschapper");

    fclose(fp);
    return 0;
}
```

Voorbeeld `output.csv` Inhoud:
```
Naam,Leeftijd,Beroep
Jane Doe,27,Datawetenschapper
```

## Diepere duik
Het CSV-formaat, hoewel schijnbaar eenvoudig, kent zijn nuances, zoals het omgaan met komma's binnen velden en het omsluiten van velden met aanhalingstekens. De rudimentaire voorbeelden getoond houden geen rekening met dergelijke complexiteiten, noch behandelen ze potentiële fouten op een robuuste manier.

Historisch gezien is de omgang met CSV in C grotendeels handmatig geweest vanwege de low-level aard van de taal en het gebrek aan ingebouwde high-level abstracties voor dergelijke taken. Dit handmatige beheer omvat het openen van bestanden, het lezen van regels, het splitsen van tekenreeksen, en het converteren van de datatypes indien nodig.

Hoewel de directe manipulatie van CSV-bestanden in C waardevolle leerervaringen biedt op het gebied van bestands-I/O en tekenreekshandeling, beloven verschillende moderne alternatieven efficiëntie en minder foutgevoelige processen. Bibliotheken zoals `libcsv` en `csv-parser` bieden uitgebreide functies voor het lezen en schrijven van CSV-bestanden, inclusief ondersteuning voor geciteerde velden en aangepaste scheidingstekens.

Als alternatief kan, wanneer men werkt binnen ecosystemen die dit ondersteunen, integreren met talen of platforms die high-level CSV-manipulatiefuncties bieden (zoals Python met zijn `pandas` bibliotheek) een productievere route zijn voor applicaties die zware CSV-verwerking vereisen. Deze cross-taalbenadering benut de prestaties en systeemprogrammeringscapaciteiten van C terwijl gebruik wordt gemaakt van het gebruiksgemak van andere talen voor specifieke taken zoals CSV-behandeling.
