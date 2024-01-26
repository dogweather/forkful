---
title:                "Arbeiten mit XML"
date:                  2024-01-26T04:27:48.589372-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit XML"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/working-with-xml.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Arbeit mit XML in C umfasst das Parsen, Erstellen und Manipulieren von XML-Dateien - also strukturierte Datenspeicherung. Programmierer tun dies, um mit Daten in einem portablen und menschenlesbaren Format zu interagieren, das oft für Konfiguration, Datenaustausch und mehr verwendet wird.

## Wie:
Unten ist ein Schnipsel, der die `libxml2`-Bibliothek zum Parsen einer XML-Datei verwendet und das Wurzelelement abruft.

```C
#include <stdio.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

int main() {
    xmlDoc *doc = NULL;
    xmlNode *root_element = NULL;

    // Die XML-Datei parsen
    doc = xmlReadFile("example.xml", NULL, 0);

    // Das Wurzelelement abrufen
    root_element = xmlDocGetRootElement(doc);

    printf("Wurzelelement: %s\n", root_element->name);

    // Das Dokument freigeben
    xmlFreeDoc(doc);

    // Parser aufräumen
    xmlCleanupParser();

    return 0;
}
```

Beispielausgabe für ein XML mit Wurzel `<data>` könnte sein:
```
Wurzelelement: data
```

## Tiefergehend
XML, oder Extensible Markup Language, geht zurück auf die späten 90er Jahre und bietet eine Möglichkeit, Daten zu beschreiben und zu strukturieren. In C ist `libxml2` das Mittel der Wahl. Es ist robust, aber nicht das einfachste für XML-Anfänger. Alternativen umfassen `tinyxml2`, das leichter und anfängerfreundlicher ist. Was die Implementierung betrifft, so hat C keine eingebaute XML-Unterstützung, daher füllen Bibliotheken diese Lücke. Sie variieren in Größe, Geschwindigkeit, Komplexität und Portabilität. Die meisten bieten DOM- und SAX-Parsing-Methoden an: DOM lädt alles in den Speicher, gut für kleine Dokumente; SAX ist ereignisgesteuert und verarbeitet Elemente on the fly, besser für große Dateien. Beide haben ihre Anwendungsfälle und Kompromisse.

## Siehe auch
- [libxml2](http://xmlsoft.org/)
- [tinyxml2 auf GitHub](https://github.com/leethomason/tinyxml2)
- [XML-Tutorial auf w3schools](https://www.w3schools.com/xml/)
- [XML-Spezifikation von W3C](https://www.w3.org/XML/)