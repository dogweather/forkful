---
title:                "C: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/working-with-json.md"
---

{{< edit_this_page >}}

# Warum

JSON (JavaScript Object Notation) ist ein beliebtes Format, um Daten auszutauschen und zu speichern. Es ist einfach zu lesen und zu schreiben, was es zu einer idealen Wahl für die Verwendung in C-Programmen macht.

# Wie man JSON in C verwendet

Die Verwendung von JSON in C erfordert die Verwendung einer externen Bibliothek. Eine der bekanntesten ist Jansson, die es uns ermöglicht, JSON-Objekte zu erstellen, zu lesen und zu speichern.

Um Jansson in unserem C-Code zu verwenden, müssen wir zunächst die Header-Datei einbinden und eine Zeile in unserem Makefile hinzufügen, die auf die Bibliothek verweist. Dann können wir JSON-Objekte erstellen, Werte hinzufügen und speichern.

Ein Beispielcode sieht wie folgt aus:

```C
#include <jansson.h>

int main() {
    // Ein neues JSON-Objekt erstellen
    json_t *person = json_object();

    // Werte hinzufügen
    json_object_set_new(person, "name", json_string("Max Mustermann"));
    json_object_set_new(person, "age", json_integer(35));
    json_object_set_new(person, "hobbies", json_array());
    json_array_append_new(json_object_get(person, "hobbies"), json_string("Lesen"));
    json_array_append_new(json_object_get(person, "hobbies"), json_string("Fußball"));

    // JSON-Objekt speichern
    json_dump_file(person, "person.json", JSON_INDENT(4));

    return 0;
}
```

Dieses Beispiel erstellt ein JSON-Objekt mit Name, Alter und einer Liste von Hobbys und speichert es in einer Datei mit dem Namen "person.json". Die Ausgabe sieht wie folgt aus:

```json
{
    "name": "Max Mustermann",
    "age": 35,
    "hobbies": [
        "Lesen",
        "Fußball"
    ]
}
```

# Tiefere Einblicke

Eine Typisierung ist in C nicht vorhanden, was bedeutet, dass wir keinen einzelnen Datentyp für alle JSON-Werte haben. Stattdessen verwendet Jansson eine Sammlung von Makros, die auf verschiedene Datentypen verweisen.

Einige der nützlichen Funktionen, die von Jansson bereitgestellt werden, sind `json_typeof()` und `json_string_value()`. Diese ermöglichen es uns, den Typ und den Wert eines JSON-Objekts oder einer Eigenschaft abzufragen.

Es ist auch wichtig zu beachten, dass Jansson standardmäßig keine Überprüfung auf ungültiges JSON durchführt. Daher ist es wichtig, unsere Eingabedaten sorgfältig zu validieren, um Fehler zu vermeiden.

# Siehe auch

- [Jansson Dokumentation](https://jansson.readthedocs.io/en/latest/)
- [JSON-C Bibliothek](https://github.com/json-c/json-c)