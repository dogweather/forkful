---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:52:36.385050-07:00
description: "Hoe: In C kan de aanwezigheid van een directory worden gecontroleerd\
  \ met de `stat` functie, die informatie ophaalt over het bestand of de directory\
  \ op een\u2026"
lastmod: '2024-03-13T22:44:51.306033-06:00'
model: gpt-4-0125-preview
summary: In C kan de aanwezigheid van een directory worden gecontroleerd met de `stat`
  functie, die informatie ophaalt over het bestand of de directory op een gespecificeerd
  pad.
title: Controleren of een directory bestaat
weight: 20
---

## Hoe:
In C kan de aanwezigheid van een directory worden gecontroleerd met de `stat` functie, die informatie ophaalt over het bestand of de directory op een gespecificeerd pad. De `S_ISDIR` macro uit `sys/stat.h` wordt vervolgens gebruikt om te evalueren of de opgehaalde informatie overeenkomt met een directory.

Hier is hoe je `stat` en `S_ISDIR` kunt gebruiken om te controleren of een directory bestaat:

```c
#include <stdio.h>
#include <sys/stat.h>

int main() {
    struct stat stats;
    
    // Pad van de te controleren directory
    char *dirPath = "/pad/naar/directory";

    // Haal de status van het pad op
    int resultaat = stat(dirPath, &stats);

    // Controleer of de directory bestaat
    if (resultaat == 0 && S_ISDIR(stats.st_mode)) {
        printf("De directory bestaat.\n");
    } else {
        printf("De directory bestaat niet.\n");
    }

    return 0;
}
```

Voorbeelduitvoer:
```
De directory bestaat.
```

Of, als de directory niet bestaat:
```
De directory bestaat niet.
```

## Diepere Duik:
De `stat` structuur en functie maken al decennia deel uit van de programmeertaal C, afkomstig van Unix. Ze bieden een gestandaardiseerde manier om informatie van het bestandssysteem op te halen, wat ondanks het relatief lage niveau, veel gebruikt wordt vanwege de eenvoud en directe toegang tot de metadata van het bestandssysteem.

Historisch gezien is het controleren van het bestaan en de eigenschappen van bestanden en directories met `stat` en zijn derivaten (zoals `fstat` en `lstat`) een gangbare aanpak geweest. Echter, deze functies interacteren direct met de OS-kernel, wat overhead en potentiele fouten kan introduceren als ze niet correct worden afgehandeld.

Voor nieuwe projecten of wanneer men werkt in high-level scenario's, kunnen programmeurs kiezen voor meer geabstraheerde mechanismen voor bestandsafhandeling die worden aangeboden door moderne frameworks of bibliotheken die fouten gracieuzer afhandelen en een eenvoudigere API bieden. Toch blijft het begrijpen en kunnen gebruiken van `stat` een waardevolle vaardigheid voor scenario’s die directe manipulatie van het bestandssysteem vereisen, zoals systeemprogrammering of wanneer werken in beperkte omgevingen waar afhankelijkheid van grote bibliotheken onhaalbaar is.
