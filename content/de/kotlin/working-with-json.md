---
title:                "Arbeiten mit json"
html_title:           "Kotlin: Arbeiten mit json"
simple_title:         "Arbeiten mit json"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

### Was & Warum?

Arbeiten mit JSON bedeutet, Daten im JSON-Format zu verarbeiten. JSON ist ein Format, das verwendet wird, um strukturierte Daten auszutauschen. Programmierer nutzen JSON, um Daten von einer Quelle zu extrahieren oder sie zu einer Quelle zu senden.

### Wie geht's?

Um mit JSON in Kotlin zu arbeiten, kannst du die Standardbibliothek von Kotlin verwenden. Hier ist ein Beispiel, wie du eine JSON-Zeichenfolge in ein Kotlin-Objekt konvertieren kannst:

```Kotlin
val jsonString = """{ "name": "Max", "age": 25, "hobby": "reading" }```
val jsonObject = JSONObject(jsonString)
val name = jsonObject.getString("name")
val age = jsonObject.getInt("age")
val hobby = jsonObject.getString("hobby")

println("Name: $name, Age: $age, Hobby: $hobby")
```

Ergebnis:
```
Name: Max, Age: 25, Hobby: reading
```

Du kannst auch objektorientierte Klassen verwenden, um JSON-Daten zu modellieren und zu verarbeiten. Hier ist ein Beispiel einer Klasse ```Person```, die Äquivalent zu dem Beispiel oben ist:

```Kotlin
class Person(val name: String, val age: Int, val hobby: String)

val jsonString = """{ "name": "Max", "age": 25, "hobby": "reading" }```
val jsonObject = JSONObject(jsonString)
val person = Person(jsonObject.getString("name"), jsonObject.getInt("age"), jsonObject.getString("hobby"))

println(person.name)
println(person.age)
println(person.hobby)
```

Ergebnis:
```
Max
25
reading
```

### Tiefer tauchen

JSON wurde entwickelt, um eine einfache Alternative zu XML zu sein. Es ist lesbarer als XML und somit für den Menschen leichter zu verstehen. JSON wird in der Regel verwendet, um Daten für den Austausch zwischen Client- und Serveranwendungen zu strukturieren und zu übertragen. Es gibt auch alternative Formate wie YAML und CSV, aber JSON gilt als Standardformat für den Datenaustausch im World Wide Web.

### Siehe auch

- [Kotlin Standardbibliothek](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/index.html)
- [JSON-Spezifikation](https://www.json.org/json-de.html)