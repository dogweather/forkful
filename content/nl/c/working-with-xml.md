---
title:                "Werken met XML"
date:                  2024-01-28T22:10:59.292884-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/working-with-xml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Werken met XML in C houdt in het parsen, creëren en manipuleren van XML-bestanden - in wezen gestructureerde gegevensopslag. Programmeurs doen dit om te interageren met gegevens in een draagbaar en leesbaar formaat, vaak gebruikt voor configuratie, gegevensuitwisseling en meer.

## Hoe:
Hieronder staat een fragment dat de `libxml2` bibliotheek gebruikt voor het parsen van een XML-bestand en het pakken van het root-element.

```C
#include <stdio.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

int main() {
    xmlDoc *doc = NULL;
    xmlNode *root_element = NULL;

    // Parse het XML-bestand
    doc = xmlReadFile("example.xml", NULL, 0);

    // Pak het root-element
    root_element = xmlDocGetRootElement(doc);

    printf("Root Element: %s\n", root_element->name);

    // Bevrijd het document
    xmlFreeDoc(doc);

    // Opruimen parser
    xmlCleanupParser();

    return 0;
}
```

Voorbeelduitvoer voor een XML met root `<data>` zou zijn:
```
Root Element: data
```

## De Diepte In
XML, of Extensible Markup Language, dateert uit de late jaren '90 en biedt een manier om gegevens te beschrijven en te structureren. In C is `libxml2` de go-to. Het is robuust, hoewel niet het gemakkelijkste voor XML-beginners. Alternatieven zijn onder meer `tinyxml2`, dat lichter en meer beginner-vriendelijk is. Wat implementatie betreft, C heeft geen ingebouwde XML-ondersteuning, dus bibliotheken vullen de lacune. Ze variëren in grootte, snelheid, complexiteit en draagbaarheid. De meeste bieden DOM- en SAX-parsing methoden: DOM laadt het hele ding in het geheugen, goed voor kleine documenten; SAX is event-gedreven, behandelt elementen on the fly, beter voor grote bestanden. Beide hebben hun gebruikscases en trade-offs.

## Zie Ook
- [libxml2](http://xmlsoft.org/)
- [tinyxml2 op GitHub](https://github.com/leethomason/tinyxml2)
- [XML-tutorial op w3schools](https://www.w3schools.com/xml/)
- [XML-specificatie door W3C](https://www.w3.org/XML/)
