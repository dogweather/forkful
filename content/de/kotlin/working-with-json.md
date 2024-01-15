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

# Warum

JSON ist heutzutage ein gängiges Format für den Austausch von Daten in vielen Anwendungen und APIs. Mit Kotlin können Sie auf einfache und effiziente Weise mit JSON umgehen und Daten in und aus JSON-Format konvertieren.

# Wie geht man vor?

Verwenden Sie die `JSONObject`- und `JSONArray`-Klassen aus dem `org.json` Package, um JSON-Objekte und -Arrays zu erstellen und zu manipulieren. Hier ist ein Beispiel, wie Sie eine JSON-Datei lesen und in ein `JSONObject`-Objekt parsen können:

```Kotlin
val jsonString = """
    {
        "name": "Max Mustermann",
        "age": 30,
        "hobbies": ["lesen", "reisen", "kochen"]
    }
""".trimIndent()

val jsonObject = JSONObject(jsonString)
println(jsonObject.getString("name")) // gibt "Max Mustermann" aus
println(jsonObject.getInt("age")) // gibt 30 aus

val hobbies = jsonObject.getJSONArray("hobbies")
println(hobbies.get(1)) // gibt "reisen" aus
```

Und hier ist ein Beispiel, wie Sie ein `JSONObject`-Objekt in eine JSON-Datei schreiben können:

```Kotlin
val person = JSONObject()
person.put("name", "Lisa Müller")
person.put("age", 25)
person.put("hobbies", listOf("malen", "tanzen", "backen"))

val jsonString = person.toString()
println(jsonString) 
// gibt '{"name":"Lisa Müller","age":25,"hobbies":["malen","tanzen","backen"]}' aus
```

# Tiefere Einblicke

- Mit `JSONObject.keys()` und `JSONObject.getJSONArray()` können Sie auf die Schlüssel und Werte eines `JSONObject`-Objekts zugreifen und sie z.B. in einer Schleife durchlaufen.
- Kotlin hat auch eine `JsonObject`-Klasse aus dem `kotlinx.serialization`-Package, die es Ihnen ermöglicht, JSON-Objekte in Datenklassen zu konvertieren und umgekehrt. Lesen Sie hier mehr darüber: https://github.com/Kotlin/kotlinx.serialization.
- Die `JSONException`-Klasse aus dem `org.json`-Package ermöglicht es Ihnen, Fehler beim Parsen von JSON-Dateien zu behandeln und benutzerdefinierte Fehlermeldungen zu generieren.

# Siehe auch

- Offizielle Dokumentation von `org.json`: https://github.com/stleary/JSON-java
- `kotlinx.serialization` Github Repository: https://github.com/Kotlin/kotlinx.serialization