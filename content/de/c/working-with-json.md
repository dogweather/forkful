---
title:                "Arbeiten mit JSON"
html_title:           "C: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/working-with-json.md"
---

{{< edit_this_page >}}

## Was & Warum?
Arbeiten mit JSON (JavaScript Object Notation) bedeutet, Daten im platzsparenden und menschenlesbaren Format auszutauschen. Programmierer verwenden JSON, um Daten zwischen verschiedenen Anwendungen oder Systemen zu übertragen, da es einfach zu verarbeiten und zu verstehen ist.

## Anleitung:
Der folgende Code zeigt, wie man mit JSON in C arbeitet:

```
#include <stdio.h>
#include <json-c/json.h>

int main() {
  // Erstellt einen JSON-Objekt-Builder
  json_object *jobj = json_object_new_object();
  
  // Fügt einen Schlüssel-Wert-Paar hinzu
  json_object *jstring = json_object_new_string("Hallo Welt!");
  json_object_object_add(jobj, "Begrüßung", jstring);
  
  // Konvertiert das Objekt in einen String
  printf("%s\n", json_object_to_json_string(jobj));
  
  // Gibt den Wert des Schlüssels "Begrüßung" aus
  json_object *jvalue;
  json_object_object_get_ex(jobj, "Begrüßung", &jvalue);
  printf("Der gespeicherte Wert ist: %s\n", json_object_get_string(jvalue));
  
  // Löscht das JSON-Objekt, um Speicher zu sparen
  json_object_put(jobj);
  
  return 0;
}
```

Die Ausgabe des obigen Codes ist:
```
{"Begrüßung": "Hallo Welt!"}
Der gespeicherte Wert ist: Hallo Welt!
```

## Tiefere Einblicke:
JSON wurde erstmals im Jahr 2001 von Douglas Crockford vorgestellt, um die Übertragung von Daten in JavaScript zu vereinfachen. Heutzutage wird es jedoch nicht nur in JavaScript, sondern in vielen anderen Programmiersprachen verwendet. Es gibt auch alternative Formate wie XML oder YAML, aber JSON ist wegen seiner Einfachheit und Lesbarkeit weiterhin sehr beliebt.

Die Implementierung von JSON in C erfolgt über die externe Bibliothek json-c. Diese muss daher zuerst installiert und dann in das Codeprojekt eingebunden werden. Weitere Informationen zur Verwendung von json-c finden Sie in der offiziellen Dokumentation.

## Siehe auch:
- [Offizielle json-c Dokumentation](https://github.com/json-c/json-c/wiki/Tutorial)
- [JSON-Spezifikation](https://www.json.org/json-de.html)