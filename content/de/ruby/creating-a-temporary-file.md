---
title:                "Erstellung einer temporären Datei"
html_title:           "Ruby: Erstellung einer temporären Datei"
simple_title:         "Erstellung einer temporären Datei"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was ist das und warum machen Programmierer das?

Das Erstellen einer temporären Datei ist ein häufiges Vorgehen in der Programmierung, bei dem vorübergehend eine Datei erstellt wird, die für spezifische Aufgaben genutzt wird. Programmierer verwenden temporäre Dateien, um Daten zu speichern, während ein Programm ausgeführt wird, oder um Informationen an andere Programme weiterzuleiten. Am Ende wird die temporäre Datei wieder gelöscht.

## Wie funktioniert es?

Um eine temporäre Datei in Ruby zu erstellen, können wir die in der "tempfile" Bibliothek eingebaute Klasse "Tempfile" nutzen. Sie erlaubt es uns, mit Dateien genauso zu arbeiten, wie wir es mit anderen Objekten in Ruby tun würden.

```ruby
require 'tempfile'
   
tempfile = Tempfile.new('meinetempdatei')
begin
  # Hier können wir Operationen auf unserer temporären Datei durchführen
ensure
  tempfile.close
  tempfile.unlink
end
```

In diesem Beispiel erstellen wir eine temporäre Datei mit dem Präfix "meinetempdatei". Danach nutzen wir die Datei innerhalb der ```begin``` und ```ensure``` Blöcke und schließen sie am Ende wieder. Das Löschen der temporären Datei ist ebenfalls wichtig, da sie ansonsten auf unserem Computer verbleiben würde.

## Tiefere Einblicke

Temporäre Dateien haben ihren Ursprung in der Computertechnologie der 1970er Jahre. Damals besaßen viele Computer keine direkte Möglichkeit, Daten zwischen Programmen auszutauschen. Also mussten temporäre Dateien genutzt werden, um Daten an andere Programme weiterzuleiten oder um sie zwischenspeichern zu können.

Alternativen zum Erstellen von temporären Dateien sind zum Beispiel das Speichern von Daten im Arbeitsspeicher oder die Nutzung von Pipes, um Informationen zwischen Programmen auszutauschen. 

Bei der Implementierung der "Tempfile" Klasse verfolgte Ruby zunächst einen anderen Ansatz, bei dem die Datei erst beim Schließen gelöscht wurde. Das führte jedoch dazu, dass in manchen Fällen versehentlich wichtige Dateien gelöscht wurden. Deshalb wurde der aktuelle Ansatz mit dem speziellen "unlink" Kommando gewählt.

## Schauen Sie auch hier vorbei 

Für weitere Informationen zu "Tempfile" in Ruby können Sie die offizielle Dokumentation unter https://ruby-doc.org/stdlib-2.6.5/libdoc/tempfile/rdoc/Tempfile.html besuchen. Sie finden auch hilfreiche Beispiele auf https://www.rubyguides.com/2015/11/ruby-tempfile/.