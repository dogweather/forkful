---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:24.626092-07:00
description: "Het verkrijgen van de huidige datum in C houdt in dat je toegang krijgt\
  \ tot de standaard C-bibliotheek om de huidige datum en tijd van het systeem op\
  \ te\u2026"
lastmod: '2024-03-11T00:14:25.164079-06:00'
model: gpt-4-0125-preview
summary: "Het verkrijgen van de huidige datum in C houdt in dat je toegang krijgt\
  \ tot de standaard C-bibliotheek om de huidige datum en tijd van het systeem op\
  \ te\u2026"
title: De huidige datum krijgen
---

{{< edit_this_page >}}

## Wat & Waarom?

Het verkrijgen van de huidige datum in C houdt in dat je toegang krijgt tot de standaard C-bibliotheek om de huidige datum en tijd van het systeem op te halen en te formatteren. Programmeurs hebben deze functionaliteit vaak nodig voor logboekregistratie, tijdstempels of planningseigenschappen binnen hun applicaties.

## Hoe te:

In C biedt de `<time.h>` header de nodige functies en typen om met datums en tijden te werken. De functie `time()` haalt de huidige tijd op, terwijl `localtime()` deze tijd converteert naar de lokale tijdzone. Om de datum te tonen, gebruiken we `strftime()` om deze als een string te formatteren.

Hier is een basisvoorbeeld:

```c
#include <stdio.h>
#include <time.h>

int main() {
    char buffer[80];
    time_t rawtime;
    struct tm *timeinfo;

    // Haal de huidige tijd op
    time(&rawtime);
    // Zet het om naar lokale tijd
    timeinfo = localtime(&rawtime);
    
    // Formateer de datum en print het
    strftime(buffer, 80, "De datum van vandaag is %Y-%m-%d", timeinfo);
    printf("%s\n", buffer);

    return 0;
}
```

Een voorbeelduitvoer kan er zo uitzien:

```
De datum van vandaag is 2023-04-12
```

## Diepere duik

De tijdafhandeling in C, zoals gefaciliteerd door `<time.h>`, gaat terug naar de vroegste dagen van de taal en UNIX-systemen. Het is gebouwd rond het `time_t` gegevenstype, dat de huidige tijd vertegenwoordigt als het aantal seconden sinds het Unix Epoch (1 januari 1970). Hoewel dit efficiënt en universeel compatibel is, betekent het ook dat de tijdfuncies van de standaard C-bibliotheek van nature beperkt zijn door het bereik en de resolutie van `time_t`.

Moderne applicaties, met name die welke timestamps met hoge resolutie vereisen of die te maken hebben met datums ver in de toekomst of het verleden, vinden deze beperkingen mogelijk lastig. Het probleem van het jaar 2038 is bijvoorbeeld een beroemde illustratie waarbij systemen die een 32-bit `time_t` gebruiken, zullen overlopen.

Voor complexere tijd- en datumbehandeling, wenden veel programmeurs zich tot externe bibliotheken of de functionaliteiten die door het besturingssysteem worden geboden. In C++, biedt de `<chrono>` bibliotheek bijvoorbeeld meer nauwkeurige en veelzijdige tijdmanipulatiemogelijkheden.

Ondanks zijn beperkingen, de eenvoud en alomtegenwoordigheid van C's tijdfuncies maken ze perfect geschikt voor veel toepassingen. Deze hulpmiddelen begrijpen is fundamenteel voor C-programmeurs en biedt een mix van historische programmeercontext en praktisch, alledaags nut.
