---
title:                "Arbeiten mit JSON"
html_title:           "Gleam: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## Warum?

Du bist entweder auf der Suche nach einer Sprache, die einfach zu verstehen und zu verwenden ist, oder du hast bereits von Gleam gehört und möchtest mehr darüber erfahren. In jedem Fall bist du hier genau richtig, denn in diesem Artikel werden wir uns mit der Verwendung von JSON in Gleam befassen.

JSON (JavaScript Object Notation) ist ein äußerst beliebtes Datenformat, das für die Übertragung von strukturierten Daten verwendet wird. In der heutigen Zeit der Webanwendungen und APIs ist es unerlässlich, sich mit JSON vertraut zu machen. Aber warum sollte man sich speziell für Gleam entscheiden? Nun, Gleam ist eine moderne, funktionale und typisierte Sprache, die es dir ermöglicht, JSON in einer einfachen und sicheren Weise zu verarbeiten.

## Wie man JSON in Gleam verwendet

Um mit JSON in Gleam zu arbeiten, benötigen wir das Paket `gleam/json`, das bereits in der Standardbibliothek von Gleam enthalten ist. Zunächst müssen wir dieses Paket importieren, indem wir `import json` am Anfang unserer Datei hinzufügen.

Als nächstes definieren wir einen JSON-String in einer Variable:

```
let json_string =
  """
  {
      "name": "John Doe",
      "age": 30,
      "hobbies": ["reading", "cooking", "hiking"],
      "is_active": true
  }
  """
```

Für eine einfache Überprüfung können wir den Inhalt dieser Variable mit `debug.print` ausgeben:

```
debug.print(json_string)
```

Die Ausgabe sollte folgendermaßen aussehen:

```
{
    "name": "John Doe",
    "age": 30,
    "hobbies": ["reading", "cooking", "hiking"],
    "is_active": true
}
```

Jetzt können wir diese JSON-Daten in ein Gleam-Modul konvertieren und die Inhalte der verschiedenen Schlüsselwörter abrufen:

```
let john = json.parse(json_string)

let name = json.get(john, "name")
let age = json.get_int(john, "age")
let hobbies = json.get_array(john, "hobbies")
let is_active = json.get_bool(john, "is_active")

debug.print("Name: {}", [name])
debug.print("Age: {}", [age])
debug.print("Hobbies: {}", [hobbies])
debug.print("Is Active: {}", [is_active])

```

Die Ausgabe sieht wie folgt aus:

```
Name: John Doe
Age: 30
Hobbies: ["reading", "cooking", "hiking"]
Is Active: true
```

Nun, was ist, wenn wir den Wert eines Schlüssels aktualisieren möchten? Das ist mit Gleam auch sehr einfach:

```
let updated_json_string = json.set(json_string, "age", 35)
```

Die Variable `updated_json_string` enthält nun den aktualisierten Wert von `age`.

## Tiefere Einblicke in die Verwendung von JSON in Gleam

Wie du sehen kannst, ist die Verarbeitung von JSON-Daten in Gleam sehr einfach und intuitiv. Aber es gibt noch weitere Möglichkeiten, wie du mit JSON in Gleam arbeiten kannst. Du kannst beispielsweise mithilfe des `from_bytes`-Befehls JSON direkt aus einem Byte-Array parsen oder mit dem `decode`-Befehl direkt in ein benutzerdefiniertes Gleam-Datentypen konvertieren.

Um mehr über die verschiedenen Funktionen und Möglichkeiten der Verwendung von JSON in Gleam zu erfahren, empfehlen wir dir, die offizielle Gleam-Dokumentation zu besuchen.

## Siehe auch

- [Gleam-Dokumentation zu JSON](https://gleam.run/documentation/standard-library/json.html)
- [JSON-Standard](https://www.json.org/json-de.html)
- [Einführung in die Funktionsprogrammierung in Gleam](https://medium.com/@lpil/introduction-to-functional-programming-with-gleam-39b28767cbbe)