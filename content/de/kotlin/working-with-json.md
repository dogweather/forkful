---
title:                "Kotlin: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

# Warum
In der heutigen digitalen Welt ist der Umgang mit Daten unerlässlich und JSON ist eine der häufigsten Datenformate. Mit Kotlin können wir JSON einfach und effizient verarbeiten.

# Wie man JSON in Kotlin verarbeitet
Um mit JSON in Kotlin zu arbeiten, müssen wir zunächst die Libary 'json' importieren. Diese stellt uns nützliche Funktionen zur Verfügung, um JSON in unsere Kotlin-Objekte zu konvertieren und umgekehrt.

```Kotlin
implementation 'org.json:json:20210307'
```

Für die Verarbeitung von JSON benötigen wir auch eine JSON-Zeichenkette. Diese können wir zum Beispiel von einer öffentlichen API abrufen oder eine Test-Zeichenkette erstellen.

```Kotlin
// Test-JSON-Zeichenkette
val json = """
    {
        "name": "Max Mustermann",
        "age": 25,
        "location": "Berlin"
    }
""".trimIndent()
```

Um diese JSON-Zeichenkette in ein Kotlin-Objekt umzuwandeln, können wir die Funktion `JSONObject()` aus der 'json' Library verwenden. Diese Funktion erwartet als Parameter die JSON-Zeichenkette und gibt ein `JSONObject` Objekt zurück.

```Kotlin
// JSON-Zeichenkette in JSON-Objekt konvertieren
val jsonObject = JSONObject(json)
```

Nun können wir auf die einzelnen Eigenschaften des JSON-Objekts zugreifen, indem wir den entsprechenden Schlüssel verwenden.

```Kotlin
// Auf Eigenschaften des JSON-Objekts zugreifen
val name = jsonObject.getString("name")  // "Max Mustermann"
val age = jsonObject.getInt("age")  // 25
val location = jsonObject.getString("location")  // "Berlin"
```

Umgekehrt können wir auch ein Kotlin-Objekt in eine JSON-Zeichenkette konvertieren. Dazu verwenden wir die Funktion `toString()` der Klasse `JSONObject`.

```Kotlin
// Kotlin-Objekt in JSON-Zeichenkette konvertieren
val newJsonString = jsonObject.toString()
```

# Tiefere Einblicke in die Arbeit mit JSON in Kotlin
Wenn wir genauer auf die `JSONObject` Klasse schauen, können wir sehen, dass es noch viele weitere nützliche Funktionen gibt, um mit JSON zu arbeiten. So können wir zum Beispiel auch verschachtelte JSON-Objekte erstellen oder auf Arrays innerhalb des JSONs zugreifen.

Auch die 'json' Library bietet noch weitere Funktionen, wie zum Beispiel die Funktion `fromJson()` um ein JSON-Objekt direkt in ein Kotlin-Objekt zu konvertieren.

Mit Kotlin können wir also auf einfache Weise JSON verarbeiten und sowohl JSON-Zeichenketten als auch Kotlin-Objekte erstellen oder konvertieren.

# Siehe auch
- [json Library Dokumentation](https://json.org/)
- [Kotlin Dokumentation zu JSON](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.json/)
- [Beispielprojekt zum Umgang mit JSON in Kotlin](https://github.com/example/json-kotlin)