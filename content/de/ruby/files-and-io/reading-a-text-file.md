---
date: 2024-01-20 17:54:58.444695-07:00
description: 'So geht''s: Um eine Datei zu lesen, nutze `File.read`, `File.readlines`
  oder den `File.open`-Block. Hier sind ein paar Beispiele.'
lastmod: '2024-03-13T22:44:54.417839-06:00'
model: gpt-4-1106-preview
summary: Um eine Datei zu lesen, nutze `File.read`, `File.readlines` oder den `File.open`-Block.
title: Textdatei einlesen
weight: 22
---

## So geht's:
Um eine Datei zu lesen, nutze `File.read`, `File.readlines` oder den `File.open`-Block. Hier sind ein paar Beispiele:

```Ruby
# Ganze Datei auf einmal lesen
inhalt = File.read('beispiel.txt')
puts inhalt

# Datei Zeile für Zeile lesen
File.readlines('beispiel.txt').each do |zeile|
  puts zeile
end

# Datei mit einem Block öffnen
File.open('beispiel.txt', 'r') do |datei|
  datei.each_line do |zeile|
    puts zeile
  end
end
```

Ausgabe könnte so aussehen:

```
Hallo Welt!
Das ist die zweite Zeile.
Hier endet die Datei.
```

## Deep Dive
Das Lesen von Textdateien ist ein grundlegendes Konzept und bereits seit den frühen Tagen der Programmierung essenziell. Vor Ruby hatten Programmiersprachen wie C oder Perl eigene Wege, um mit Dateien zu arbeiten. Ruby hat die Arbeit mit Dateien stark vereinfacht.

Alternativen zum Lesen von Dateien in Ruby sind I/O-Streams (`IO`-Klasse) und niedrigere Level-Operationen mit `syscall`. Für größere Dateien ist es effizienter, den Inhalt zeilenweise über einen `File.open`-Block zu verarbeiten, damit nicht der gesamte Inhalt im Speicher gehalten wird.

Beim Implementieren solcher Funktionen muss man auch Fehlerbehandlung bedenken. Zum Beispiel sollten Sie Ausnahmen behandeln, falls Dateien nicht existieren oder nicht gelesen werden können:

```Ruby
begin
  File.readlines('nicht_existierende_datei.txt')
rescue Errno::ENOENT => e
  puts "Datei wurde nicht gefunden: #{e}"
end
```

## See Also
- Die offizielle Ruby-Dokumentation zu File-Klassenmethoden: [Ruby-Docs File](https://ruby-doc.org/core/File.html)
- Ein Rundgang durch Ruby I/O: [Ruby I/O](https://www.tutorialspoint.com/ruby/ruby_input_output.htm)
- Fehlerbehandlung in Ruby: [Ruby Exception Handling](https://ruby-doc.org/core-2.5.1/Exception.html)
