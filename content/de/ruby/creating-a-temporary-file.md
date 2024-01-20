---
title:                "Eine temporäre Datei erstellen"
html_title:           "Java: Eine temporäre Datei erstellen"
simple_title:         "Eine temporäre Datei erstellen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Erstellen einer temporären Datei in Ruby bedeutet, eine Datei für kurze Zeit zu erstellen und zu verwenden. Programmierer machen das, um Daten kurzfristig zu speichern oder um mit anderen Anwendungen zusammenzuarbeiten, ohne dauerhafte Änderungen vorzunehmen.

## So geht's:

Schaffung einer temporären Datei mit dem `Tempfile` Modul:

```Ruby
require 'tempfile'

datei = Tempfile.new('datei')
puts datei.path
datei.write('Hallo, Ruby!')
datei.rewind
puts datei.read
datei.close
```

In diesem Code `Tempfile.new('datei')` erstellt eine neue temporäre Datei, `write` schreibt in diese Datei, `rewind` kehrt zum Dateianfang zurück, und `read` ruft die geschriebenen Daten ab. Schließlich schließt `close` die Datei.

## Vertiefung:

Die Fähigkeit, temporäre Dateien zu erstellen ist eine weit verbreitete Funktion in den meisten Programmiersprachen. In Ruby ist das `Tempfile` Modul seit seiner Version 1.8 vorhanden.

- **Alternativen**: Eine weitere Option könnte sein, eine reguläre Datei mit der Methode `File.new` zu erstellen und sie nach Gebrauch zu löschen.
- **Umsetzungsdetails**: Temporäre Dateien in Ruby werden in einem speziellen Ordner auf Ihrem Computer erstellt. Der genaue Ort variiert je nach Betriebssystem.

## Siehe auch:

- Ruby-Dokumentation zum `Tempfile` Modul: [https://ruby-doc.org/stdlib-3.1.0/libdoc/tempfile/rdoc/Tempfile.html](https://ruby-doc.org/stdlib-3.1.0/libdoc/tempfile/rdoc/Tempfile.html)
- Weiterführende Informationen zur Datei- und E/A-Verarbeitung in Ruby: [https://ruby-doc.org/core-3.1.0/File.html](https://ruby-doc.org/core-3.1.0/File.html)