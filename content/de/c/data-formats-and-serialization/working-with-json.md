---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:49.247478-07:00
description: "Die Arbeit mit JSON (JavaScript Object Notation) in C umfasst das Parsen,\
  \ Generieren und Manipulieren von JSON-Datenstrukturen. Programmierer tun dies,\
  \ um\u2026"
lastmod: '2024-03-13T22:44:54.376538-06:00'
model: gpt-4-0125-preview
summary: Die Arbeit mit JSON (JavaScript Object Notation) in C umfasst das Parsen,
  Generieren und Manipulieren von JSON-Datenstrukturen.
title: Arbeiten mit JSON
weight: 38
---

## Was & Warum?

Die Arbeit mit JSON (JavaScript Object Notation) in C umfasst das Parsen, Generieren und Manipulieren von JSON-Datenstrukturen. Programmierer tun dies, um die Kommunikation mit Webdiensten, Datenspeicherung oder Konfigurationsdateien in einem leichtgewichtigen und menschenlesbaren Format zu ermöglichen.

## Wie:

Um mit JSON in C zu arbeiten, verwenden Sie typischerweise eine Bibliothek wie `jansson` oder `json-c`, da C keine integrierte Unterstützung für JSON bietet. Hier konzentrieren wir uns auf `jansson` aufgrund seiner Benutzerfreundlichkeit und aktiver Wartung. Zuerst installieren Sie die Bibliothek (z.B. mit einem Paketmanager wie `apt` auf Ubuntu: `sudo apt-get install libjansson-dev`).

Beginnen wir damit, einen JSON-String zu parsen und auf seinen Inhalt zuzugreifen:

```c
#include <jansson.h>
#include <stdio.h>

int main() {
    const char *json_string = "{\"name\":\"John Doe\",\"age\":30}";
    json_error_t error;
    json_t *root = json_loads(json_string, 0, &error);
    
    if(!root) {
        fprintf(stderr, "Fehler: in Zeile %d: %s\n", error.line, error.text);
        return 1;
    }
    
    const char *name;
    int alter;
    json_unpack(root, "{s:s, s:i}", "name", &name, "age", &alter);
    
    printf("Name: %s\nAlter: %d\n", name, alter);
    
    json_decref(root);
    return 0;
}
```

Beispielausgabe:
```
Name: John Doe
Alter: 30
```

Als Nächstes erstellen und geben wir ein JSON-Objekt aus:

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

Beispielausgabe:
```
{"name": "Jane Doe", "age": 25}
```

Diese Beispiele demonstrieren die Grundlagen des Ladens eines JSON-Strings, das Auspacken seiner Werte, das Erstellen eines neuen JSON-Objekts und anschließend dessen Ausgabe als String.

## Tiefergehender Einblick

Die Notwendigkeit, mit JSON in C zu arbeiten, ergibt sich aus der Annahme von JSON als primäres Format für den Datenaustausch im Web. Die Einfachheit und Effizienz von JSON ließen es schnell XML überholen, trotz der anfänglichen Abwesenheit von direkter Unterstützung für JSON-Manipulation in C. Frühe Lösungen beinhalteten manuelle Stringmanipulationen - fehleranfällig und ineffizient. Bibliotheken wie `jansson` und `json-c` entstanden, um diese Lücke zu schließen, und bieten robuste APIs für das Parsen, Konstruieren und Serialisieren von JSON.

Während `jansson` Einfachheit und Benutzerfreundlichkeit bietet, könnte `json-c` diejenigen ansprechen, die nach einem breiteren Funktionsumfang suchen. Dennoch bieten Alternativen wie Parsing-Bibliotheken in C++ aufgrund der komplexeren Datenstrukturen und der Unterstützung durch die Standardbibliothek dieses Sprache ausgefeiltere Abstraktionen. Wenn jedoch in Umgebungen gearbeitet wird, in denen C die bevorzugte oder erforderliche Sprache ist - wie in eingebetteten Systemen oder beim Schnittstellen mit bestehenden C-Bibliotheken - wird die Verwendung von `jansson` oder `json-c` unverzichtbar.

Es ist auch erwähnenswert, dass die Arbeit mit JSON in C ein tieferes Verständnis für Speicherverwaltung erfordert, da diese Bibliotheken häufig dynamisch zugewiesene Objekte zurückgeben, die eine explizite Freigabe erfordern. Dies stellt Programmierer vor die Herausforderung, Bequemlichkeit mit der Verantwortung zu vereinbaren, Speicherlecks zu vermeiden, ein entscheidender Aspekt beim Schreiben effizienten C-Codes.
