---
aliases:
- /nl/c/capitalizing-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:52:50.382197-07:00
description: "Een string met hoofdletters schrijven in C houdt in dat je het eerste\
  \ teken van elk woord in een gegeven string omzet naar een hoofdletter als het een\u2026"
lastmod: 2024-02-18 23:09:02.353526
model: gpt-4-0125-preview
summary: "Een string met hoofdletters schrijven in C houdt in dat je het eerste teken\
  \ van elk woord in een gegeven string omzet naar een hoofdletter als het een\u2026"
title: Een string kapitaliseren
---

{{< edit_this_page >}}

## Wat & Waarom?

Een string met hoofdletters schrijven in C houdt in dat je het eerste teken van elk woord in een gegeven string omzet naar een hoofdletter als het een kleine letter is. Programmeurs voeren deze bewerking vaak uit om gebruikersinvoer te standaardiseren voor zoekopdrachten, sorteerbewerkingen of weergavedoeleinden, om consistentie en leesbaarheid in tekstgegevens te waarborgen.

## Hoe te:

Het met hoofdletters schrijven van een string in C vereist een basisbegrip van karaktermanipulatie en het doorlopen van strings. Aangezien C geen ingebouwde functie hiervoor heeft, zul je doorgaans elk teken controleren, en indien nodig de hoofdletter aanpassen. Hieronder staat een eenvoudige implementatie:

```c
#include <stdio.h>
#include <ctype.h> // Voor de functies islower en toupper

void capitalizeString(char *str) {
    if (str == NULL) return; // Veiligheidscontrole
    
    int capNext = 1; // Vlag om aan te geven of de volgende letter een hoofdletter moet zijn
    for (int i = 0; str[i] != '\0'; i++) {
        if (capNext && islower(str[i])) {
            str[i] = toupper(str[i]); // Zet teken om naar hoofdletter
            capNext = 0; // Reset vlag
        } else if (str[i] == ' ') {
            capNext = 1; // Volgende teken moet een hoofdletter zijn
        }
    }
}

int main() {
    char exampleString[] = "hello world. programming in c!";
    capitalizeString(exampleString);
    printf("String met hoofdletters: %s\n", exampleString);
    return 0;
}
```

Voorbeelduitvoer:
```
String met hoofdletters: Hello World. Programming In C!
```

Dit programma doorloopt de string `exampleString`, controleerend elk teken of het met een hoofdletter moet worden geschreven. De functie `islower` controleert of een teken een kleine letter is, terwijl `toupper` het omzet naar een hoofdletter. De vlag `capNext` bepaalt of de volgende letter die wordt aangetroffen omgezet dient te worden, deze wordt ingesteld na elke spatie (' ') die wordt gevonden, en aanvankelijk om het eerste teken van de string een hoofdletter te maken.

## Diepere Duik

De getoonde techniek is eenvoudig maar mist efficiëntie voor zeer grote strings of wanneer ze herhaaldelijk wordt uitgevoerd in prestatie-kritieke toepassingen. In historische en implementatie-contexten omvat stringmanipulatie in C, inclusief kapitalisatie, vaak directe buffermanipulatie, wat de low-level benadering van C weerspiegelt en de programmeur volledige controle geeft over geheugen en prestatie-afwegingen.

Er zijn alternatieve, meer geavanceerde methoden voor het met hoofdletters schrijven van strings, vooral wanneer rekening wordt gehouden met landinstellingen en unicode-tekens, waar kapitalisatieregels aanzienlijk kunnen verschillen van het eenvoudige ASCII-scenario. Bibliotheken zoals ICU (International Components for Unicode) bieden robuuste oplossingen voor deze gevallen, maar introduceren afhankelijkheden en overhead die mogelijk niet noodzakelijk zijn voor alle toepassingen.

Verder, hoewel het voorbeeld de C Standard Library-functies `islower` en `toupper` gebruikt, die deel uitmaken van `<ctype.h>`, is het essentieel om te begrijpen dat deze werken binnen het ASCII-bereik. Voor toepassingen die verwerking van karakters buiten ASCII vereisen, zoals het hanteren van accenttekens in Europese talen, zal aanvullende logica of externe bibliotheken nodig zijn om kapitalisatie nauwkeurig uit te voeren.

Concluderend, hoewel de uiteengezette methode geschikt is voor veel toepassingen, is het cruciaal om de beperkingen en de beschikbare alternatieven te begrijpen voor het ontwikkelen van robuuste, geïnternationaliseerde software in C.
