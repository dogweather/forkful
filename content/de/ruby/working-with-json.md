---
title:                "Arbeiten mit JSON"
html_title:           "Ruby: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich mit JSON beschäftigen? Ganz einfach: JSON ist eine äußerst praktische Methode, um Daten zu übertragen und zu speichern. Es ist leicht lesbar, flexibel und platzsparend.

## Wie geht es?

Um mit JSON in Ruby zu arbeiten, müssen wir zuerst das "json" Modul importieren. Das können wir mit dem folgenden Code tun:

```Ruby
require 'json'
```

Als nächstes müssen wir ein JSON-Objekt erstellen, in dem wir unsere Daten speichern können. Wir können dies entweder manuell tun oder die ".to_json" Methode verwenden, um ein Hash-Objekt automatisch in JSON zu konvertieren. Hier ist ein Beispiel:

```Ruby
# manuell
my_data = {"Name" => "Max", "Alter" => 27, "Ort" => "Berlin"}

# automatisch
my_hash = {a: 1, b: 2, c: 3}
my_json = my_hash.to_json
```

Um unsere Daten in JSON-Format zu speichern, können wir die ".to_json" Methode auch auf größere Datenstrukturen wie Arrays oder Objekte anwenden. Hier ist ein Beispiel mit einem Array:

```Ruby
my_array = ["Ruby", 2.6, true]
my_array.to_json # => "[\"Ruby\",2.6,true]"
```

Um JSON-Daten zu lesen, können wir die ".parse" Methode verwenden und die Daten in ein Hash-Objekt konvertieren. Hier ist ein Beispiel:

```Ruby
my_json = '{"Name": "Anna", "Alter": 30, "Ort": "Hamburg"}'
my_hash = JSON.parse(my_json)
puts my_hash["Name"] # => "Anna"
puts my_hash["Alter"] # => 30
puts my_hash["Ort"] # => "Hamburg"
```

## Tiefer eintauchen

Nun, da wir wissen, wie wir JSON-Daten in Ruby erstellen und lesen können, lassen Sie uns einen tieferen Einblick in die Arbeit mit JSON erhalten.

Um die Lesbarkeit von JSON-Code zu verbessern, können wir die ".pretty_generate" Methode verwenden, um unseren Daten automatisch Einzüge und Zeilenumbrüche hinzuzufügen. Hier ist ein Beispiel:

```Ruby
my_hash = {x: 1, y: 2, z: 3}
puts JSON.pretty_generate(my_hash)
# =>
#{
#  "x": 1,
#  "y": 2,
#  "z": 3
#}
```

Wenn wir mit komplexen Datenstrukturen wie verschachtelten Arrays oder Objekten arbeiten, können wir die ".generate" Methode verwenden, um unsere Daten in eine gültige JSON-Zeichenfolge zu konvertieren. Hier ist ein Beispiel:

```Ruby
my_array = ["Ruby", {version: 2.6, year: 2018}, true]
JSON.generate(my_array) # => "[\"Ruby\",{\"version\":2.6,\"year\":2018},true]"
```

Es ist auch wichtig zu wissen, dass Ruby automatisch Datentypen wie Strings, Zahlen und Wahrheitswerte im JSON-Format konvertiert. Zum Beispiel wird ein "String"-Objekt zu einem JSON-String und ein "Fixnum"-Objekt zu einer JSON-Zahl. Um ein bestimmtes Datentypen zu erhalten, können wir die ".to_json" Methode mit einem zusätzlichen Argument verwenden. Hier ist ein Beispiel:

```Ruby
my_hash = {name: "Tom", age: 42}
my_json = my_hash.to_json # => "{\"name\":\"Tom\",\"age\":42}"
my_json = my_hash.to_json(string_options: {ascii_only: true}) # => "{\"name\":\"Tom\",\"age\":42}"
```

## Siehe auch

Weitere Informationen und Tutorials zu Ruby und JSON finden Sie unter:

- [Ruby Dokumentation](https://www.ruby-lang.org/de/documentation/)
- [Offizielle JSON-Seite](https://www.json.org/json-de.html)
- [JSON in Ruby](https://hackhands.com/ruby-read-json-file-hash/)
- [Video-Tutorial zu JSON in Ruby](https://www.youtube.com/watch?v=_vUnWfztTfM)

Happy Coding! 💻