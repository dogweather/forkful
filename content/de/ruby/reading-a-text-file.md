---
title:                "Eine Textdatei lesen"
html_title:           "Ruby: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum
Textdateien sind eine häufig genutzte Methode, um Daten und Informationen zu speichern. Als Programmierer ist es daher wichtig zu wissen, wie man Textdateien lesen und verarbeiten kann. Diese Fähigkeit ermöglicht es uns, effizienter zu arbeiten und komplexe Aufgaben zu lösen.

## Wie man Textdateien in Ruby liest
In Ruby gibt es mehrere Möglichkeiten, eine Textdatei zu lesen. Eine davon ist die Verwendung der `File`-Klasse. Hier ist ein Beispiel, wie man eine Textdatei mit dem Namen "data.txt" öffnen und lesen kann:

```Ruby
File.open("data.txt", "r") do |file|
  while line = file.gets
    puts line
  end
end
```

In diesem Beispiel verwenden wir die Methode `open` der `File`-Klasse, um die Datei zu öffnen. Der erste Parameter gibt den Dateinamen an, der zweite Parameter zeigt an, dass wir die Datei nur lesen ("r" für "read") möchten. Innerhalb des `do`-Blocks verwenden wir die Methode `gets`, um eine Zeile aus der Datei zu lesen. Diese Zeile wird dann einfach mit `puts` ausgegeben. Der `do`-Block sorgt dafür, dass die Datei nach dem Lesen automatisch geschlossen wird.

## Tiefer Einblick
Die `File`-Klasse hat noch weitere nützliche Methoden, um mit Textdateien zu arbeiten. Zum Beispiel gibt es die Methode `readlines`, die alle Zeilen aus einer Datei als Array zurückgibt. Oder die Methode `read`, die den gesamten Inhalt einer Datei als String zurückgibt. Auch das Schreiben in eine Textdatei ist mit der `File`-Klasse möglich, indem man den zweiten Parameter beim Öffnen auf "w" für "write" setzt.

Es ist auch möglich, Textdateien mit der `CSV`-Klasse zu lesen und zu schreiben, wenn sie im CSV-Format vorliegen. Dies ermöglicht eine einfache Verarbeitung von Tabellen und Datenbanken.

## Siehe auch
- [Dokumentation der File-Klasse](https://ruby-doc.org/core-2.7.2/File.html)
- [Dokumentation der CSV-Klasse](https://ruby-doc.org/stdlib-2.7.2/libdoc/csv/rdoc/CSV.html)
- [Ruby für Einsteiger: Dateien](https://www.ruby-einsteiger.de/programmieren-dateien.html)