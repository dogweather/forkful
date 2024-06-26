---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:43.211143-07:00
description: 'Hoe te: In C wordt de standaard bibliotheekfunctie `strlen()` vaak gebruikt
  om de lengte van een reeks te vinden. Hier is een snel voorbeeld.'
lastmod: '2024-03-13T22:44:51.281112-06:00'
model: gpt-4-0125-preview
summary: In C wordt de standaard bibliotheekfunctie `strlen()` vaak gebruikt om de
  lengte van een reeks te vinden.
title: De lengte van een string vinden
weight: 7
---

## Hoe te:
In C wordt de standaard bibliotheekfunctie `strlen()` vaak gebruikt om de lengte van een reeks te vinden. Hier is een snel voorbeeld:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char myString[] = "Hello, World!";
    size_t length = strlen(myString);
    
    printf("Lengte van '%s' is %zu.\n", myString, length);
    
    return 0;
}
```

**Voorbeelduitvoer:**
```
Lengte van 'Hello, World!' is 13.
```

In dit voorbeeld neemt `strlen()` een reeks (`myString`) als invoer en retourneert de lengte exclusief de null-terminator. Het gebruik van `size_t` voor de lengte variabele wordt aanbevolen omdat het een ongetekend geheel getaltype is, waardoor het in staat is de grootte van het grootst mogelijke object op het systeem te vertegenwoordigen.

## Diepgaand:
De functie `strlen()` maakt deel uit van de C-standaardbibliotheek sinds de aanvang van de taal. Onder de motorkap werkt het door een teller te verhogen terwijl het de reeks doorloopt totdat het de null-terminator bereikt. Deze eenvoud komt echter met prestatieoverwegingen: omdat `strlen()` tekens in realtime telt, is het herhaaldelijk aanroepen ervan op dezelfde reeks in een lus bijvoorbeeld inefficiënt.

Wat betreft beveiliging, `strlen()` en andere C-reeksbehandelfuncties controleren niet inherent op bufferoverlopen, waardoor zorgvuldig programmeren essentieel is om kwetsbaarheden te vermijden. Moderne alternatieven in andere talen, zoals reekstypes die de lengte omvatten of standaard veilige bufferafhandeling gebruiken, elimineren enkele van deze risico's en inefficiënties.

Ondanks de beperkingen is het begrijpen van `strlen()` en het handmatig behandelen van reeksen in C cruciaal voor programmeurs, vooral wanneer er met low-level code gewerkt wordt of wanneer prestatie en geheugenbeheer van het grootste belang zijn. Het biedt ook waardevolle inzichten in de werking van hogere-niveau reeksabstracties in andere talen.
