---
title:    "Ruby: Erstellen einer temporären Datei"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum du beim Programmieren temporäre Dateien erstellen möchtest. Manchmal benötigst du temporäre Speicherplätze, um Daten zu speichern, die für deine Anwendung oder Berechnungen benötigt werden. Oder du möchtest eine neue Datei erstellen, die nur vorübergehend benötigt wird und danach gelöscht werden kann. Egal aus welchem Grund, das Erstellen von temporären Dateien kann deine Arbeit als Ruby-Programmierer optimieren und dir helfen, dein Ziel schneller zu erreichen.

## Wie geht's

Das Erstellen einer temporären Datei ist in Ruby relativ einfach und kann mit der Standardbibliothek "tempfile" erledigt werden. Du kannst einfach die Methode "tempfile" aufrufen und dabei die gewünschte Dateiendung und das Präfix für deine Datei angeben. Hier ist ein Beispielcode, der eine temporäre CSV-Datei erstellt:

```Ruby
require 'tempfile'

# Eine temporäre CSV-Datei mit dem Präfix "temp" erstellen
temp_file = Tempfile.new(['temp', '.csv'])

# Daten in die Datei schreiben
temp_file.puts("Name,Alter,Geschlecht")
temp_file.puts("Max,25,männlich")
temp_file.puts("Lisa,30,weiblich")

# Die Datei schließen und speichern
temp_file.close

# Ausgabe des Dateinamens
puts temp_file.path
```

Die Ausgabe dieses Codes wäre eine temporäre CSV-Datei mit dem Namen "temp-12345.csv", wobei die Zahlen "12345" automatisch generiert werden.

## Tiefentauchen

Wenn du tiefer in die Thematik eintauchen möchtest, bietet die "tempfile" Bibliothek viele Optionen, um deine temporären Dateien anzupassen. Zum Beispiel kannst du die Größe der Buffer ändern oder die Löschung deiner temporären Datei manuell steuern. Außerdem gibt es auch die Möglichkeit, den Speicherort und den Namen der temporären Datei selbst zu bestimmen. Um mehr darüber zu erfahren, kannst du die offizielle Dokumentation zu "tempfile" lesen oder den Source Code der Bibliothek auf GitHub untersuchen.

## Siehe auch

- [tempfile Dokumentation](https://ruby-doc.org/stdlib-2.6.3/libdoc/tempfile/rdoc/Tempfile.html)
- [tempfile Source Code auf GitHub](https://github.com/ruby/tempfile)