---
title:                "Arbeiten mit CSV-Dateien"
html_title:           "Arduino: Arbeiten mit CSV-Dateien"
simple_title:         "Arbeiten mit CSV-Dateien"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## Was & Warum?
CSV steht für "Comma-Separated Values". Es ist ein einfaches Format, um Tabellendaten zu speichern und zu transportieren, besonders wenn man es mit Datenbanken oder Tabellenkalkulationen zu tun hat. Programmierer nutzen CSV, weil es universell lesbar ist und leicht zu bearbeitende Struktur hat.

## So geht's:
Ruby macht das Arbeiten mit CSV-Dateien zum Kinderspiel. Du brauchst die eingebaute CSV-Bibliothek nicht mal extra zu installieren. Hier ein kurzes Beispiel:

```Ruby
require 'csv'

# CSV-Datei lesen
CSV.foreach("beispiel.csv") do |row|
  puts row.inspect
end

# CSV-Datei schreiben
CSV.open("ausgabe.csv", "w") do |csv|
  csv << ["Name", "Stadt", "Alter"]
  csv << ["Max", "Berlin", 29]
  csv << ["Zoe", "Hamburg", 22]
end
```
Diese beiden Code-Snippets lesen eine CSV-Datei namens `beispiel.csv` und schreiben eine neue CSV-Datei namens `ausgabe.csv` mit ein paar Daten.

## Deep Dive
Die CSV-Unterstützung in Ruby ist tief verwurzelt - bereits seit 1.8 ist sie dabei. Alternativen wie FasterCSV sind

von Ruby 1.9 an eigentlich unnötig, da FasterCSV die Basis für die eingebaute CSV-Bibliothek wurde. Details zur Implementierung: Ruby's CSV-Bibliothek handhabt auch komplexere CSV-Features wie unterschiedliche Zeichenkodierungen, benutzerdefinierte Trennzeichen und Zeilenumbrüche, so dass sie sehr flexibel ist.

## See Also
- Ruby-Dokumentation zur CSV-Bibliothek: https://ruby-doc.org/stdlib-2.6.1/libdoc/csv/rdoc/CSV.html
- Schneller Einstieg in CSV mit Ruby: https://www.rubyguides.com/2018/10/parse-csv-ruby/
- Komplexere CSV-Aufgaben mit Ruby: https://thoughtbot.com/blog/faster-csv-processing-with-ruby
