---
title:                "Eine Textdatei schreiben"
date:                  2024-01-19
html_title:           "Arduino: Eine Textdatei schreiben"
simple_title:         "Eine Textdatei schreiben"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Textdateien zu schreiben bedeutet, Daten in eine lesbare Datei zu übertragen. Programmierer nutzen dies, um Daten zu persistieren, Log-Informationen zu speichern oder Konfigurationen zu teilen.

## How to:

Ruby macht es einfach, Inhalte in eine Textdatei zu schreiben. Hier sind einige Beispiele:

```Ruby
# Eine neue Textdatei erstellen und schreiben
File.open('beispiel.txt', 'w') do |file|
  file.puts("Hallo Welt!")
end

# In eine bestehende Textdatei schreiben
File.open('beispiel.txt', 'a') do |file|
  file.puts("Eine weitere Zeile.")
end

# Kurze Schreibweise mit einer Zeile
File.write('beispiel.txt', "Schnelles Schreiben!", mode: 'a')
```

Sample Output in 'beispiel.txt':

```
Hallo Welt!
Eine weitere Zeile.
Schnelles Schreiben!
```

## Deep Dive:

Das Erstellen von Textdateien in Ruby basiert auf den Prinzipien der Input-Output-Streams, ein Konzept, das bis auf die frühen Tage der Programmierung zurückgeht. Alternativen zum Schreiben in Textdateien können Datenbanken, Key-Value-Stores oder beispielsweise Cloud-Speicherdienste sein. Ruby nutzt dabei ihre Klasse `File`, die eine Abstraktion des unterliegenden Betriebssystems File I/O-Interfaces bietet und dadurch plattformunabhängig funktioniert.

## See Also:

- Ruby-Dokumentation zur `File`-Klasse: [ruby-doc.org/core/File.html](https://ruby-doc.org/core/File.html)
- Einführung in Ruby I/O: [ruby-lang.org/de/documentation/ruby-from-other-languages/to-ruby-from-c-and-cpp/](https://www.ruby-lang.org/de/documentation/ruby-from-other-languages/to-ruby-from-c-and-cpp/)
