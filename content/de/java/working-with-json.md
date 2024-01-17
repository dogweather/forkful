---
title:                "Arbeiten mit JSON"
html_title:           "Java: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/working-with-json.md"
---

{{< edit_this_page >}}

# Was ist JSON und warum verwenden Programmierer es?

JSON (JavaScript Object Notation) ist ein Datenformat, das häufig in der Programmierung verwendet wird, um strukturierte Daten zu speichern und auszutauschen. Es ist ein leicht lesbares und leichtgewichtiges Format, das auf einer einfachen Syntax basiert. Programmierer verwenden JSON, um Daten zwischen verschiedenen Anwendungen auszutauschen, da es plattformübergreifend und gut lesbar ist.

# Wie funktioniert JSON in Java?

Die Verwendung von JSON in Java ist ziemlich einfach und erfordert nur wenige Zeilen Code. Zunächst muss die JSON-Bibliothek in das Projekt eingebunden werden. Anschließend können Daten in Form von JSON-Objekten erstellt und ausgelesen werden. Hier ist ein Beispiel, wie man einen JSON-String in ein Java-Objekt umwandelt:

```java
// Verwendung der JSON Bibliothek
import org.json.*;

// Erstellen eines JSON-Strings
String jsonStr = "{\"name\": \"Max\", \"age\": 25}";

// Umwandeln des Strings in ein JSONObject
JSONObject obj = new JSONObject(jsonStr);

// Auslesen und Verwenden der Daten
String name = obj.getString("name");
int age = obj.getInt("age");
System.out.println("Name: " + name);
System.out.println("Alter: " + age);
```

Die Ausgabe dieses Codes wäre:

```
Name: Max
Alter: 25
```

# Tiefere Einblicke in JSON

JSON wurde in den späten 2000er Jahren entwickelt, um eine alternative Datenformatoption zu bieten, da XML (Extensible Markup Language) zu komplex und überladen wurde. Es gibt auch alternative Datenformate wie YAML (Yet Another Markup Language) und CSV (Comma-Separated Values), aber JSON hat sich aufgrund seiner Lesbarkeit und Einfachheit als sehr beliebt erwiesen.

Die Java-Bibliothek für JSON ist robust und gut dokumentiert, was sie zu einer hervorragenden Wahl für die Verwendung von JSON in Java macht. Sie bietet auch Funktionen wie Fehlerbehandlung und Validierung, um sicherzustellen, dass die ausgetauschten Daten korrekt verarbeitet werden.

# Weitere Quellen

- Offizielle JSON-Website: https://www.json.org/
- Java-Bibliothek für JSON: https://github.com/stleary/JSON-java
- Vergleich von JSON mit anderen Datenformaten: https://www.toptal.com/web/the-json-advantage