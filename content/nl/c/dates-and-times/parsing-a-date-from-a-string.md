---
title:                "Een datum ontleden uit een string"
date:                  2024-02-03T18:00:13.097833-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een datum ontleden uit een string"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het ontleden van een datum uit een string in C betreft het omzetten van tekstuele voorstellingen van data naar een formaat dat programma's effectiever kunnen manipuleren en analyseren. Dit is cruciaal voor taken zoals datumrekenwerk, vergelijkingen en het formatteren voor verschillende locales, aangezien het programmeurs in staat stelt om gebruikersinvoer of datasetvermeldingen op een gestandaardiseerde manier te behandelen.

## Hoe:

C biedt geen ingebouwde manier om direct data uit strings te ontleden, dus maken we vaak gebruik van de `strptime` functie die beschikbaar is in de `<time.h>` bibliotheek voor POSIX-systemen. Deze functie stelt ons in staat om het verwachte formaat van de invoerstring te specificeren en deze te ontleden naar een `struct tm`, die een kalenderdatum en -tijd in zijn componenten opsplitst.

Hier is een simpel voorbeeld van hoe je `strptime` kunt gebruiken om een datum uit een string te ontleden:

```c
#include <time.h>
#include <stdio.h>

int main() {
    const char *dateStr = "2023-04-01";
    struct tm tm;
    char buf[255];

    // Het ontleden van de datumstring naar struct tm
    if (strptime(dateStr, "%Y-%m-%d", &tm) == NULL) {
        printf("Het ontleden van de datum is mislukt.\n");
    } else {
        // Gebruik van strftime om de datum in een leesbaar formaat te printen
        strftime(buf, sizeof(buf), "%A, %B %d, %Y", &tm);
        printf("Ontlede datum: %s\n", buf);
    }

    return 0;
}
```

Voorbeelduitvoer voor dit programma zou zijn:

```
Ontlede datum: Zaterdag, April 01, 2023
```

Het is essentieel om potentiële fouten af te handelen, zoals `strptime` dat het patroon niet kan matchen of onverwachte invoer tegenkomt.

## Diepgaande duik

De `strptime` functie, hoewel krachtig, maakt geen deel uit van de standaard C-bibliotheek en wordt voornamelijk gevonden op POSIX-conforme systemen zoals Linux en UNIX. Deze beperking betekent dat programma's die afhankelijk zijn van `strptime` voor het ontleden van data uit strings mogelijk niet draagbaar zijn naar niet-POSIX-systemen zoals Windows zonder aanvullende compatibiliteitslagen of bibliotheken.

Historisch gezien vereiste het hanteren van datums en tijden in C veel handmatige manipulatie en zorg, vooral gezien verschillende locales en tijdzones. Moderne alternatieven en uitbreidingen voor C, zoals de C++ `<chrono>` bibliotheek en externe bibliotheken zoals Howard Hinnant's date-bibliotheek voor C++, bieden robuustere oplossingen voor datum- en tijdsmanipulatie, inclusief ontleding. Deze bibliotheken bieden doorgaans betere ondersteuning voor een bredere reeks datumformaten, tijdzones en foutafhandelingsmechanismen, wat ze de voorkeur geeft voor nieuwe projecten die uitgebreide datum- en tijdmanipulatiecapaciteiten vereisen.

Niettemin kan het begrijpen hoe je datums uit strings in C kunt ontleden voordelig zijn, vooral bij het werken aan of onderhouden van projecten die compatibel moeten zijn met systemen waar deze moderne hulpmiddelen niet beschikbaar zijn of wanneer gewerkt wordt binnen de beperkingen van strikte C-programmeeromgevingen.
