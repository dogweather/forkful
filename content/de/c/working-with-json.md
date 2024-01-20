---
title:                "Arbeiten mit JSON"
html_title:           "Arduino: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/working-with-json.md"
---

{{< edit_this_page >}}

## Was & Warum?
JSON ist ein Format zum Speichern und Übertragen von Daten. Entwickler nutzen es, weil es einfach zu lesen ist und von vielen Programmiersprachen unterstützt wird.

## How to:
Du kannst in C die `json-c` Bibliothek benutzen. Hier ist ein einfaches Beispiel:

```C
#include <json-c/json.h>
#include <stdio.h>

int main() {
    // JSON Objekt erzeugen
    json_object *jobj = json_object_new_object();
    
    // JSON Daten hinzufügen
    json_object_object_add(jobj, "name", json_object_new_string("John Doe"));
    json_object_object_add(jobj, "age", json_object_new_int(29));

    // JSON als String ausgeben
    printf("JSON string: %s\n", json_object_to_json_string(jobj));

    // Speicher freigeben
    json_object_put(jobj);
    
    return 0;
}
```

Ausgabe:

```
JSON string: {"name": "John Doe", "age": 29}
```

## Deep Dive
JSON (JavaScript Object Notation) wurde Anfang der 2000er Jahre populär. Es ist einfacher als XML und braucht keine DTD oder Schemata. Alternativen wie YAML sind für komplexe Hierarchien besser, aber JSON ist für Web APIs Standard. Was die Implementierung in C angeht, setzt man auf Bibliotheken wie `json-c` oder `Jansson`, die das Parsen und Erstellen von JSON erleichtern.

## See Also
- JSON-C GitHub: https://github.com/json-c/json-c
- Jansson Bibliothek: http://www.digip.org/jansson/
- Offizielle JSON Webseite: https://www.json.org/json-de.html