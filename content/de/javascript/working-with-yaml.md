---
title:                "Arbeiten mit YAML"
html_title:           "Javascript: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

Was ist YAML und warum benutzen Programmierer es?
YAML steht für "YAML Ain't Markup Language" und ist eine textbasierte Datenformatierungssprache. Programmierer verwenden YAML, um Daten in einem einfachen und leicht lesbaren Format zu speichern und weiterzugeben. Es ist besonders nützlich für die Konfiguration von Software, da es sowohl von Menschen als auch von Maschinen gelesen werden kann.

Wie geht das?
Die Syntax von YAML ist einfach und minimal, ähnlich wie bei einer Textdatei. Es verwendet Einrückungen und Zeilenumbrüche, um die Datenstruktur zu definieren. Hier ist ein Beispiel für ein YAML-Dokument, das Informationen über ein Buch speichert:

```Javascript
title: "The Hitchhiker's Guide to the Galaxy"
author: Douglas Adams
published: 1979
genre: Science Fiction
```

Es gibt auch die Möglichkeit, verschachtelte Datenstrukturen zu erstellen, indem man Bindestriche für Listen und Doppelpunkte für Schlüssel-Wert-Paare verwendet. Hier ist ein Beispiel für eine Liste von Büchern:

```Javascript
- title: "1984"
  author: George Orwell
  published: 1949
  genre: Dystopian Fiction
- title: "Brave New World"
  author: Aldous Huxley
  published: 1932
  genre: Dystopian Fiction
```

Tipp: Um zu überprüfen, ob Ihr YAML-Dokument gültig ist, können Sie einen Online-YAML-Validator wie diesen verwenden: https://yaml-online-parser.appspot.com/

Tiefer in die Materie eintauchen
YAML wurde ursprünglich entwickelt, um eine einfache Alternative zu XML zu sein und wurde erstmals im Jahr 2001 veröffentlicht. Es wird häufig in Kombination mit anderen Programmiersprachen wie Ruby und Python verwendet, um Konfigurationsdateien zu erstellen. Es gibt auch andere Datenformate wie JSON und XML, die ähnliche Zwecke erfüllen, aber YAML ist aufgrund seiner einfachen Syntax und Lesbarkeit bei vielen Programmierern beliebt.

Siehe auch
- Offizielle YAML-Website: https://yaml.org/
- YAML-Spezifikation: https://yaml.org/spec/1.2/spec.html
- Vergleich von YAML, JSON und XML: https://stackify.com/yaml-vs-json-vs-xml-format-comparison/