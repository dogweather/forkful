---
title:                "Werken met JSON"
aliases:
- /nl/c/working-with-json/
date:                  2024-02-03T18:11:57.031715-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Werken met JSON (JavaScript Object Notation) in C houdt in dat je JSON-gegevensstructuren moet parsen, genereren en manipuleren. Programmeurs doen dit om communicatie mogelijk te maken met webservices, gegevensopslag of configuratiebestanden in een lichtgewicht en menselijk leesbaar formaat.

## Hoe te:

Om met JSON in C te werken, gebruik je doorgaans een bibliotheek zoals `jansson` of `json-c` vanwege het ontbreken van ingebouwde ondersteuning voor JSON in C. Hier zullen we ons richten op `jansson` vanwege het gebruiksgemak en actief onderhoud. Installeer eerst de bibliotheek (bijvoorbeeld met een pakketbeheerder zoals `apt` op Ubuntu: `sudo apt-get install libjansson-dev`).

Laten we beginnen met het parsen van een JSON-string en het toegang krijgen tot de inhoud ervan:

```c
#include <jansson.h>
#include <stdio.h>

int main() {
    const char *json_string = "{\"name\":\"John Doe\",\"age\":30}";
    json_error_t error;
    json_t *root = json_loads(json_string, 0, &error);
    
    if(!root) {
        fprintf(stderr, "error: op regel %d: %s\n", error.line, error.text);
        return 1;
    }
    
    const char *name;
    int age;
    json_unpack(root, "{s:s, s:i}", "name", &name, "age", &age);
    
    printf("Naam: %s\nLeeftijd: %d\n", name, age);
    
    json_decref(root);
    return 0;
}
```

Voorbeelduitvoer:
```
Naam: John Doe
Leeftijd: 30
```

Vervolgens een JSON-object creëren en uitvoeren:

```c
#include <jansson.h>
#include <stdio.h>

int main() {
    json_t *root = json_object();
    json_object_set_new(root, "name", json_string("Jane Doe"));
    json_object_set_new(root, "age", json_integer(25));
    
    char *json_dump = json_dumps(root, JSON_ENCODE_ANY);
    printf("%s\n", json_dump);
    
    free(json_dump);
    json_decref(root);
    return 0;
}
```

Voorbeelduitvoer:
```
{"name": "Jane Doe", "age": 25}
```

Deze voorbeelden demonstreren de basis van het laden van een JSON-string, het uitpakken van de waarden, een nieuw JSON-object creëren, en dit vervolgens uitvoeren als een string.

## Diepere Duik

De noodzaak om met JSON in C te werken komt voort uit de acceptatie van JSON door het web als het primaire formaat voor gegevensuitwisseling. De eenvoud en efficiëntie van JSON zorgden ervoor dat het snel XML overtrof, ondanks de aanvankelijke afwezigheid van C in directe ondersteuning voor JSON-manipulatie. Eerdere oplossingen omvatten handmatige stringmanipulatie - foutgevoelig en inefficiënt. Bibliotheken zoals `jansson` en `json-c` kwamen op de markt om deze kloof te overbruggen, en boden robuuste API's voor het parsen, construeren en serialiseren van JSON.

Terwijl `jansson` eenvoud en gebruiksgemak biedt, kan `json-c` aantrekkelijk zijn voor degenen die op zoek zijn naar een breder scala aan functies. Desalniettemin bieden alternatieven zoals parseer-bibliotheken in C++ meer geavanceerde abstracties, dankzij de complexere gegevensstructuren en ondersteuning van de standaardbibliotheek van die taal. Echter, wanneer je werkt in omgevingen waar C de geprefereerde of vereiste taal is - zoals ingebedde systemen of bij het koppelen met bestaande C-bibliotheken - wordt het gebruik van `jansson` of `json-c` onmisbaar.

Het is ook belangrijk om op te merken dat werken met JSON in C een dieper begrip van geheugenbeheer met zich meebrengt, aangezien deze bibliotheken vaak dynamisch toegewezen objecten retourneren die een expliciete deallocatie vereisen. Dit daagt programmeurs uit om gemak te balanceren met de verantwoordelijkheid om geheugenlekken te voorkomen, een cruciaal aspect van het vervaardigen van efficiënte C-code.
