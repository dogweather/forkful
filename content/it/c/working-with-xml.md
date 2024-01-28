---
title:                "Lavorare con XML"
date:                  2024-01-26T04:27:59.257281-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con XML"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/working-with-xml.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?
Lavorare con XML in C comporta l'analisi, la creazione e la manipolazione di file XML - fondamentalmente lo storage di dati strutturati. I programmatori lo fanno per interagire con i dati in un formato portatile e leggibile dall'uomo, spesso utilizzato per configurazione, scambio di dati e altro.

## Come fare:
Di seguito è riportato un frammento che utilizza la libreria `libxml2` per analizzare un file XML e acquisire l'elemento radice.

```C
#include <stdio.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

int main() {
    xmlDoc *doc = NULL;
    xmlNode *root_element = NULL;

    // Analizzare il file XML
    doc = xmlReadFile("example.xml", NULL, 0);

    // Ottenere l'elemento radice
    root_element = xmlDocGetRootElement(doc);

    printf("Elemento Radice: %s\n", root_element->name);

    // Liberare il documento
    xmlFreeDoc(doc);

    // Pulire il parser
    xmlCleanupParser();

    return 0;
}
```

Un esempio di output per un XML con radice `<data>` potrebbe essere:
```
Elemento Radice: data
```

## Approfondimento
XML, o Extensible Markup Language, risale alla fine degli anni '90, fornendo un modo per descrivere e strutturare i dati. In C, `libxml2` è la scelta prediletta. È robusto, anche se non il più facile per i neofiti di XML. Tra le alternative c'è `tinyxml2`, che è più leggero e più adatto ai principianti. Per quanto riguarda l'implementazione, C non ha il supporto incorporato per XML, quindi le librerie colmano questa lacuna. Variano in dimensioni, velocità, complessità e portabilità. La maggior parte offre metodi di parsing DOM e SAX: DOM carica l'intera cosa in memoria, buono per documenti piccoli; SAX è basato sugli eventi, gestendo gli elementi al volo, meglio per file grandi. Entrambi hanno i loro casi d'uso e compromessi.

## Vedi Anche
- [libxml2](http://xmlsoft.org/)
- [tinyxml2 su GitHub](https://github.com/leethomason/tinyxml2)
- [Tutorial su XML su w3schools](https://www.w3schools.com/xml/)
- [Specifiche XML da W3C](https://www.w3.org/XML/)
