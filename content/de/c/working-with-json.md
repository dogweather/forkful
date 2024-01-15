---
title:                "Arbeiten mit json"
html_title:           "C: Arbeiten mit json"
simple_title:         "Arbeiten mit json"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/working-with-json.md"
---

{{< edit_this_page >}}

## Warum
JSON (JavaScript Object Notation) ist ein gängiges Format für den Austausch von Daten in Webanwendungen. Es ist beliebt aufgrund seiner Einfachheit und Lesbarkeit, was es zu einer guten Wahl für die Datenkommunikation macht.

## Wie geht das?
Um mit JSON in C zu arbeiten, gibt es einige wichtige Dinge zu beachten. Zunächst müssen Sie die Header-Datei "json-c/json.h" in Ihr Programm einbinden. Dann können Sie die verschiedenen Funktionen dieser Bibliothek verwenden, um JSON-Objekte zu erstellen, zu analysieren und zu manipulieren.

Hier ist ein Beispiel für die Erstellung und Manipulation eines einfachen JSON-Objekts:

```
#include <json-c/json.h>

int main() {
    // JSON-Objekt erstellen
    json_object *student = json_object_new_object();

    // Name und Alter hinzufügen
    json_object *name = json_object_new_string("Max Mustermann");
    json_object_object_add(student, "name", name);
    json_object *age = json_object_new_int(25);
    json_object_object_add(student, "age", age);

    // JSON-Objekt in einen String konvertieren und ausgeben
    const char *json_str = json_object_to_json_string(student);
    printf("%s\n", json_str);

    // Alter ändern
    json_object *new_age = json_object_new_int(30);
    json_object_object_add(student, "age", new_age);

    // JSON-Objekt erneut ausgeben
    json_str = json_object_to_json_string(student);
    printf("%s\n", json_str);

    // JSON-Objekt freigeben
    json_object_put(student);

    return 0;
}
```

Die Ausgabe dieses Codes wäre:

```
{"name": "Max Mustermann", "age": 25}
{"name": "Max Mustermann", "age": 30}
```

Weitere Informationen zu den verfügbaren Funktionen finden Sie in der offiziellen Dokumentation von [json-c](https://github.com/json-c/json-c).

## Tiefer eintauchen
Die json-c-Bibliothek bietet auch Unterstützung für komplexere JSON-Datenstrukturen wie Arrays und verschachtelte Objekte. Diese können mit den entsprechenden Funktionen wie "json_object_new_array" und "json_object_object_add" erstellt und manipuliert werden.

Ein weiterer wichtiger Punkt ist die Verwendung von "json_object_put", um ein JSON-Objekt freizugeben, sobald es nicht mehr benötigt wird. Dies ist besonders wichtig, um Speicherlecks zu vermeiden, die zu unerwünschten Problemen führen können.

Es ist auch wichtig zu beachten, dass JSON schreibgeschützt ist, was bedeutet, dass es nicht möglich ist, die Werte in einem JSON-Objekt direkt zu ändern. Stattdessen müssen Sie ein neues Objekt mit den aktualisierten Werten erstellen und das alte freigeben.

## Siehe auch
- Offizielle Dokumentation von [json-c](https://github.com/json-c/json-c)
- Einfache C-Implementierung von [json-parser](https://github.com/udp/json-parser)
- Video-Tutorial zur Verwendung von JSON in C von [Tech with Tim](https://youtu.be/Ux9EdsWFrCU)