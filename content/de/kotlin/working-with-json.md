---
title:                "Arbeiten mit JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## Was & Warum?

JSON (JavaScript Object Notation) ist ein leichtgewichtiges Datenformat zum Austausch von Daten. Entwickler nutzen es, weil es menschenlesbar und maschinenverarbeitbar ist - perfekt für APIs und Konfigurationen.

## Wie geht das:

In Kotlin kannst du mit der `kotlinx.serialization` Bibliothek JSON problemlos handhaben. Hier ist ein Beispiel, wie du ein JSON-Objekt parsen kannst:

```Kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

@Serializable
data class User(val name: String, val age: Int)

fun main() {
    val json = """{"name": "Max", "age": 25}"""
    val user = Json.decodeFromString<User>(json)
    println(user)
}
```

Ergebnis:

```
User(name=Max, age=25)
```

Und so serialisiert du ein Kotlin-Objekt zu JSON:

```Kotlin
fun main() {
    val user = User("Max", 25)
    val json = Json.encodeToString(user)
    println(json)
}
```

Ergebnis:

```
{"name":"Max","age":25}
```

## Deep Dive

JSON wurde Anfang der 2000er als Alternative zu XML entwickelt und hat wegen seiner Einfachheit schnell an Beliebtheit gewonnen. Kotlin bietet mit `kotlinx.serialization` eine moderne Bibliothek, die auch mit Multiplattform-Projekten gut funktioniert. Als Alternativen kommen Gson oder Moshi infrage, beide sind ebenfalls in der Kotlin Welt verbreitet.

## Siehe auch:

- Official kotlinx.serialization documentation: [https://github.com/Kotlin/kotlinx.serialization](https://github.com/Kotlin/kotlinx.serialization)
- Gson GitHub: [https://github.com/google/gson](https://github.com/google/gson)
- Moshi GitHub: [https://github.com/square/moshi](https://github.com/square/moshi)
- Kotlin Programming Language: [https://kotlinlang.org/](https://kotlinlang.org/)
