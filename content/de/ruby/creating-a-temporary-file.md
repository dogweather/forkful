---
title:    "Ruby: Ein temporäres Datei erstellen"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Das Erstellen von temporären Dateien ist eine nützliche Technik in der Ruby Programmierung, um vorübergehende Daten zu speichern und zu verarbeiten. Es kann besonders hilfreich sein, wenn es darum geht, große Datenmengen zu verarbeiten oder wenn man mit sensiblen Daten arbeitet, die nicht dauerhaft gespeichert werden sollen.

## Wie erstellt man eine temporäre Datei

Zur Erstellung einer temporären Datei in Ruby gibt es verschiedene Methoden, aber die einfachste und gängigste ist die Verwendung der Ruby Standardbibliothek `Tempfile`. Diese Klasse bietet eine einfache Möglichkeit, temporäre Dateien zu erstellen und zu verwalten.

Um eine temporäre Datei zu erstellen, müssen wir zunächst die Bibliothek in unser Skript einbinden:

```Ruby
require 'tempfile'
```

Dann können wir eine Instanz der `Tempfile` Klasse erstellen und spezifizieren, in welchem Verzeichnis die temporäre Datei gespeichert werden soll:

```Ruby
file = Tempfile.new('tempfile', './tmp')
```

In diesem Fall wird die temporäre Datei mit dem Präfix "tempfile" im Unterverzeichnis "tmp" erstellt. Wir können auch einen Suffix angeben, der standardmäßig die Endung ".tmp" hat. Die Datei wird automatisch gelöscht, sobald die Instanz der Tempfile Klasse verworfen wird.

Um Daten in die temporäre Datei zu schreiben, können wir die `write` Methode verwenden:

```Ruby
file.write("Beispieltext für die temporäre Datei")
```

Und um den Inhalt der Datei zu lesen, können wir die `read` Methode nutzen:

```Ruby
puts file.read # Ausgabe: "Beispieltext für die temporäre Datei"
```

## Tiefergehende Informationen

Die `Tempfile` Klasse bietet weitere nützliche Methoden, wie z.B. das Ändern des Dateinamens, das Abrufen des absoluten Dateipfads oder das Löschen der Datei.

Außerdem können wir mithilfe von Blocken sicherstellen, dass die temporäre Datei automatisch nach der Ausführung des Codes gelöscht wird:

```Ruby
Tempfile.open('tempfile', './tmp') do |file|
  # Code für die Verarbeitung der temporären Datei
end
```

## Weitere Informationen

- [Dokumentation der Tempfile Klasse (englisch)](https://ruby-doc.org/stdlib-2.7.1/libdoc/tempfile/rdoc/Tempfile.html)
- [Tutorial zur Arbeit mit temporären Dateien in Ruby (englisch)](https://www.rubyguides.com/2017/06/ruby-temporary-files/)
- [Beispielprojekt für die Verwendung temporärer Dateien in Ruby (englisch)](https://github.com/ankane/groupcount)