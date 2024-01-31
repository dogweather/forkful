---
title:                "Tekst zoeken en vervangen"
date:                  2024-01-28T22:07:04.476215-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tekst zoeken en vervangen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/searching-and-replacing-text.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Zoeken en vervangen van tekst in programmeren houdt in dat specifieke strings worden gevonden en vervangen door iets anders - denk aan de functie "zoeken en vervangen" in je tekstverwerker, maar dan voor code. Programmeurs gebruiken dit om code te refactoren, gegevens te manipuleren en bewerkingen te automatiseren die handmatig tijdrovend zouden zijn.

## Hoe:

Laten we praktisch aan de slag gaan met code. We gebruiken `strstr()` om te zoeken en `strcpy()` om te vervangen. Hier is een eenvoudig C-programma:

```C
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void searchAndReplace(char *text, const char *search, const char *replace) {
    char *pos, temp[1024];
    int index = 0;
    int searchLen = strlen(search);

    temp[0] = '\0'; // Zorg ervoor dat temp leeg is

    // Doorloop de tekst om alle voorkomens van de zoekstring te vinden
    while ((pos = strstr(text, search)) != NULL) {
        // Kopieer de tekst die leidt tot de zoekstring
        strncpy(temp + index, text, pos - text);
        index += pos - text;
        
        // Voeg de vervangingstekst toe
        strcpy(temp + index, replace);
        index += strlen(replace);
        
        // Ga voorbij de zoekstring in de tekst
        text = pos + searchLen;
    }
    
    // Voeg eventuele resterende tekst toe
    strcpy(temp + index, text);

    // Output het resultaat
    printf("Vervangen tekst: %s\n", temp);
}

int main() {
    char text[] = "De regen in Spanje valt voornamelijk op de vlakte.";
    char search[] = "in";
    char replace[] = "op";

    searchAndReplace(text, search, replace);

    return 0;
}
```
Voorbeelduitvoer:
```
Vervangen tekst: De regen op Spanje valt voornamelijk op de vlakte.
```

## Diepgaand

Historisch gezien is tekstverwerking een oud concept, dat teruggaat tot hulpmiddelen zoals `sed` in Unix. C heeft geen ingebouwde "zoek en vervang"-functie; daarom combineren we stringfuncties.

Alternatieven voor onze aanpak omvatten reguliere expressies (regex) – krachtig maar complex – en externe bibliotheken die mogelijk meer flexibiliteit bieden.

Het begrijpen van pointers, geheugentoewijzing en bufferbeheer is cruciaal; anders riskeer je problemen zoals bufferoverlopen. Een grondige implementatie controleert op dergelijke fouten en is afgestemd op prestatie voor grote teksten of frequente operaties.

## Zie Ook

Voor meer context en geavanceerde gebruiksscenario's, bekijk:

- C Standard Library documentatie over stringbehandeling: http://www.cplusplus.com/reference/cstring/
- GNU `sed` voor streambewerking: https://www.gnu.org/software/sed/
- Regex tutorial voor patroonmatching: https://www.regular-expressions.info/tutorial.html
- Uitleg van pointers in C: http://cslibrary.stanford.edu/102/
