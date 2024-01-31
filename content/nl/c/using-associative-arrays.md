---
title:                "Gebruik van associatieve arrays"
date:                  2024-01-30T19:10:41.470755-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gebruik van associatieve arrays"
programming_language: "C"
category:             "C"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Associatieve arrays, of hash maps, zijn sleutel-waardeparen waarmee je gegevens kunt opslaan en ophalen met een sleutel. Ze zijn ongelooflijk nuttig in C omdat ze snellere gegevenstoegang mogelijk maken in vergelijking met lijsten, vooral wanneer je te maken hebt met een grote hoeveelheid gegevens.

## Hoe dan:

C heeft geen ingebouwde ondersteuning voor associatieve arrays zoals sommige andere talen, maar we kunnen structuren en een aantal bibliotheekfuncties gebruiken om vergelijkbare functionaliteit te krijgen. Hier is een eenvoudige implementatie met de `uthash` bibliotheek, die je in je project moet opnemen.

Definieer eerst een structuur om je sleutel-waardeparen in op te slaan:

```C
#include <stdio.h>
#include "uthash.h"

typedef struct {
    int id; // Dit wordt onze sleutel
    char name[10]; // Dit is de waarde geassocieerd met onze sleutel
    UT_hash_handle hh; // Maakt deze structuur hashbaar
} persoon;
```

Laten we vervolgens enkele items toevoegen en ophalen:

```C
int main() {
    persoon *mijn_mensen = NULL, *s;

    // Een item toevoegen
    s = (persoon*)malloc(sizeof(persoon));
    s->id = 1;
    strcpy(s->name, "Alice");
    HASH_ADD_INT(mijn_mensen, id, s);

    // Een item ophalen
    int gebruiker_id = 1;
    HASH_FIND_INT(mijn_mensen, &gebruiker_id, s);
    if (s) {
        printf("Gevonden: %s\n", s->name);
    }
    
    return 0;
}
```

Voorbeelduitvoer zou zijn:

```
Gevonden: Alice
```

Vergeet niet om toegewezen geheugen vrij te maken en de hash-tafel te deallokeren wanneer je klaar bent om geheugenlekken te voorkomen.

## Diepere Duik

Hoewel associatieve arrays niet native zijn in C, vullen bibliotheken zoals `uthash` de kloof vrij goed op en bieden ze een vrij eenvoudige manier om deze functionaliteit te gebruiken. Historisch gezien moesten C-ontwikkelaars hun versie van deze gegevensstructuren implementeren, wat leidde tot gevarieerde en vaak complexe implementaties, vooral voor diegenen die net beginnen met de taal.

Onthoud, de efficiëntie van het gebruik van associatieve arrays in C hangt grotendeels af van hoe goed de hash-functie waarden over de tafel verdeelt om botsingen te minimaliseren. Hoewel bibliotheken zoals `uthash` een goede balans bieden tussen gebruiksgemak en prestaties, wil je in kritieke toepassingen waar prestaties van het grootste belang zijn, misschien je eigen hash-tafel op maat maken of implementeren.

Voor toepassingen die maximale efficiëntie vereisen, kunnen alternatieve gegevensstructuren of zelfs andere programmeertalen met ingebouwde ondersteuning voor associatieve arrays een betere keuze zijn. Echter, voor veel situaties, vooral waar je al binnen een C-omgeving werkt, biedt het gebruik van een bibliotheek zoals `uthash` een praktische balans tussen prestaties en gemak.
