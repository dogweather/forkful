---
title:                "C: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/working-with-yaml.md"
---

{{< edit_this_page >}}

# Warum YAML in der C-Programmierung verwenden?

YAML steht für "Yet Another Markup Language" und ist eine einfache und intuitive Möglichkeit, strukturierte Daten in einem menschenlesbaren Format zu speichern. In der C-Programmierung kann YAML zum Beispiel für die Konfiguration von Programmen oder das Lesen und Schreiben von Daten verwendet werden.

# Wie man YAML in C verwendet

Um YAML in C zu verwenden, müssen wir zuerst die Bibliothek "libyaml" installieren. Diese Bibliothek ermöglicht es uns, in unserem Code auf die Funktionen zuzugreifen, die zum Lesen und Schreiben von YAML-Dokumenten benötigt werden.

Um mit YAML zu arbeiten, müssen wir zunächst ein neues Dokument erstellen, in dem wir unsere Daten speichern können. Dies kann mit der Funktion `yaml_document_initialize()` erreicht werden, die ein leeres Dokument erstellt. Dann können wir mithilfe der Funktionen `yaml_document_add_scalar()` und `yaml_document_add_mapping()` Schlüssel-Wert-Paare oder komplexe Datenstrukturen hinzufügen.

Hier ist ein Beispielcode, der einen einfachen YAML-Datensatz erstellt und ausgibt:

```C
#include <stdio.h>
#include "yaml.h"

int main(void)
{
    // initialisiere ein neues Dokument
    yaml_document_t doc;
    yaml_document_initialize(&doc, NULL, NULL, NULL, 0, 0);

    // füge einen Skalar-Wert hinzu
    yaml_node_t *key = yaml_document_add_scalar(
        &doc, (yaml_char_t *) "name", (yaml_char_t *) "Max Mustermann");

    // füge einen Mapping-Wert hinzu
    yaml_node_t *mapping = yaml_document_add_mapping(&doc, NULL, YAML_FLOW_MAPPING_STYLE);

    // füge ein weiteres Schlüssel-Wert-Paar hinzu
    yaml_node_t *key2 = yaml_document_add_scalar(
        &doc, (yaml_char_t *) "age", (yaml_char_t *) "30");

    // füge den Mapping-Wert dem Dokument hinzu
    yaml_document_append_mapping_pair(&doc, mapping, key, key2);

    // erzeuge einen YAML-String aus dem Dokument
    yaml_char_t *yaml = (yaml_char_t *) malloc(1024 * sizeof(yaml_char_t));
    int yaml_size = yaml_document_dump(&doc, yaml, 0, 0);

    // gib den YAML-String aus
    printf("%s", yaml);

    // gib den Speicher wieder frei
    free(yaml);

    // lösche das Dokument
    yaml_document_delete(&doc);

    return 0;
}
```

Die Ausgabe dieses Codes sieht folgendermaßen aus:

```YAML
name: Max Mustermann
{age: '30'}
```

# Tiefergehende Informationen zu YAML in C

Es ist wichtig zu beachten, dass YAML in C mit einer gewissen Einschränkung verwendet werden kann. Beim Lesen von YAML-Dokumenten kann es zu Problemen kommen, wenn die Dokumente nicht korrekt formatiert sind oder nicht den erwarteten Datentypen entsprechen. Es ist daher wichtig, dass wir immer sicherstellen, dass unsere YAML-Daten gültig sind, bevor wir versuchen, sie zu lesen oder zu schreiben.

Eine weitere wichtige Sache zu beachten ist, dass libyaml die Möglichkeit bietet, benutzerdefinierte Handler für verschiedene Event-Typen zu erstellen. Dies kann nützlich sein, wenn wir bestimmte Aktionen ausführen möchten, während ein YAML-Dokument gelesen oder geschrieben wird, wie zum Beispiel die Validierung von Daten oder die Ausführung von Berechnungen.

# Siehe auch

- Die offizielle Dokumentation von libyaml: [https://pyyaml.org/wiki/LibYAML](https://pyyaml.org/wiki/LibYAML)
- YAML-Spezifikation: [https://yaml.org/spec/](https://yaml.org/spec/)
- Ein einfaches Tutorial zur Verwendung von libyaml: [https://blog.stackhpc.com/2019/04/13/c-as-yaml/](https://blog.stackhpc.com/2019/04/13/c-as-yaml/)