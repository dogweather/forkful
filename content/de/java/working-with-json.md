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

## Warum

JSON (JavaScript Object Notation) ist ein gängiges Format zum Austausch von Daten zwischen Anwendungen. Es ist sowohl einfach zu lesen und zu schreiben als auch plattformunabhängig, was es zu einer beliebten Wahl für die Datenübertragung macht.

## So geht's

Die Arbeit mit JSON in Java ist relativ einfach. Hier sind einige Beispiele und Ausgaben, um dir den Einstieg zu erleichtern.

Um eine JSON-Datei zu lesen, kann das JSONObject verwendet werden. Dazu muss die JSON-Datei zuerst als String eingelesen werden. Dann wird der String dem JSONObject-Konstruktor übergeben.

```Java
// Vorraussetzung: import org.json.JSONObject;
String jsonString = "{\"name\":\"Max\", \"age\":25}";
JSONObject json = new JSONObject(jsonString);
```

Um auf bestimmte Werte in der JSON-Datei zuzugreifen, können die entsprechenden Methoden verwendet werden. Zum Beispiel, um auf den Namen zuzugreifen:

```Java
String name = json.getString("name");
System.out.println(name); // Ausgabe: Max
```

Um eine JSON-Datei zu schreiben, kann das JSONObject verwendet werden, um die Einträge hinzuzufügen. Dann kann die toJSONString()-Methode verwendet werden, um die Datei als String zu erhalten.

```Java
JSONObject json = new JSONObject();
json.put("name", "Max");
json.put("age", 25);
String jsonString = json.toJSONString();
System.out.println(jsonString); // Ausgabe: {"name":"Max", "age":25}
```

## Tiefentauchen

JSON bietet auch die Möglichkeit, komplexe Datenstrukturen zu erstellen, indem Objekte und Arrays in Objekten und Arrays verschachtelt werden. Zum Beispiel:

```Java
{
  "name": "Max",
  "age": 25,
  "hobbies": ["Gaming", "Reading", "Hiking"],
  "address": {
    "street": "Hauptstraße 123",
    "city": "Berlin",
    "zip": 12345
  }
}
```

Hier kann auf die Liste der Hobbys zugegriffen werden, indem man auf das entsprechende Array zugreift:

```Java
JSONArray hobbies = json.getJSONArray("hobbies");
String hobby = hobbies.getString(0);
System.out.println(hobby); // Ausgabe: Gaming
```

Und auf die Straße der Adresse kann folgendermaßen zugegriffen werden:

```Java
JSONObject address = json.getJSONObject("address");
String street = address.getString("street");
System.out.println(street); // Ausgabe: Hauptstraße 123
```

Es gibt viele weitere Methoden, die beim Arbeiten mit JSON hilfreich sind, wie z.B. die getJSONArray(), getJSONObject(), putIfAbsent() und viele andere. Es ist empfehlenswert, die offizielle Java-Dokumentation für JSONObject und JSONArray zu konsultieren, um alle verfügbaren Methoden zu erfahren.

## Siehe auch

- [Java Dokumentation für JSONObject](https://docs.oracle.com/javase/7/docs/api/org/json/JSONObject.html)
- [Java Dokumentation für JSONArray](https://docs.oracle.com/javase/7/docs/api/org/json/JSONArray.html)
- [JSON-Tutorial für Anfänger](https://www.json.org/json-de.html)