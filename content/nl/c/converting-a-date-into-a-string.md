---
title:                "Een datum converteren naar een string"
aliases:
- nl/c/converting-a-date-into-a-string.md
date:                  2024-02-03T17:54:03.270702-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een datum converteren naar een string"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/converting-a-date-into-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het omzetten van een datum naar een tekenreeks in C betreft het vertalen van een datumstructuur of tijdstempel naar een voor mensen leesbaar formaat. Programmeurs voeren deze taak vaak uit om datums weer te geven in logs, gebruikersinterfaces, of wanneer datums in een tekstgebaseerd formaat zoals JSON of CSV worden opgeslagen.

## Hoe te:

De `strftime` functie uit de `<time.h>` bibliotheek wordt hier vaak voor gebruikt. Het stelt je in staat om datum en tijd op verschillende manieren te formatteren door opmaakspecifiers op te geven. Hier is een snel voorbeeld:

```c
#include <stdio.h>
#include <time.h>

int main() {
    char dateStr[100];
    time_t now = time(NULL);
    struct tm *ptm = localtime(&now);

    // Converteer de datum & tijd naar tekenreeks (bijv. "Wed Jun 30 21:49:08 2021")
    strftime(dateStr, sizeof(dateStr), "%a %b %d %H:%M:%S %Y", ptm);
    
    printf("Huidige Datum en Tijd: %s\n", dateStr);
    return 0;
}
```

Voorbeelduitvoer zou er zo uit kunnen zien:

```
Huidige Datum en Tijd: Wed Jun 30 21:49:08 2021
```

Je kunt het formaat aanpassen door de opmaakspecifiers die aan `strftime` worden doorgegeven te veranderen. Om bijvoorbeeld de datum in het formaat `YYYY-MM-DD` te krijgen, zou je `"%Y-%m-%d"` gebruiken.

## Diepgaande duik

De `strftime` functie en de `<time.h>` bibliotheek zijn onderdeel van de C Standard Library, die teruggaat tot de originele ANSI C standaard (C89/C90). Hoewel eenvoudig en ondersteund op vele platforms, kan deze benadering laagdrempelig en omslachtig lijken in vergelijking met moderne programmeertalen die intuïtievere datum- en tijdbibliotheken bieden.

Men moet opmerken, hoewel de tijd functies van de C standaardbibliotheek algemeen ondersteund worden en relatief eenvoudig te gebruiken zijn, missen ze enkele van de meer complexe tijdzone manipulatie- en internationaliseringsfuncties die te vinden zijn in bibliotheken van nieuwere talen of in third-party C bibliotheken zoals International Components for Unicode (ICU).

Echter, de aanpassingsmogelijkheden van de `strftime` functie en de brede platformondersteuning maken het een betrouwbaar en nuttig hulpmiddel voor datumreeksconversie in C. Programmeurs afkomstig van talen met hogere datumbibliotheken moeten misschien wennen aan de lage instapniveau, maar zullen het opmerkelijk krachtig en veelzijdig vinden voor het formatteren van datums en tijden voor een verscheidenheid aan toepassingen.
