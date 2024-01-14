---
title:                "Ruby: Erstellen einer temporären Datei"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum
Wenn du programmierst, wirst du oft auf die Notwendigkeit stoßen, temporäre Dateien zu erstellen. Diese Dateien werden nur vorübergehend benötigt und dienen dazu, Daten oder andere Informationen zwischenspeichern zu können. Das Erstellen von temporären Dateien kann sehr nützlich sein, um Speicherplatz zu sparen oder um Daten zu verarbeiten, die nicht in der endgültigen Datei gespeichert werden sollen.

## Anleitung
Das Erstellen einer temporären Datei in Ruby ist einfach und erfordert nur wenige Zeilen Code. Zunächst musst du die "tempfile" Library einbinden und eine neue Instanz erstellen, die als Parameter einen Dateinamen und den Modus "w+" (Schreiben und Lesen) erhält. Innerhalb der tempfile-Instanz kannst du nun wie gewohnt Dateien in Ruby erstellen, öffnen und bearbeiten. Hier ein Beispiel:

```Ruby
require 'tempfile'

temp_file = Tempfile.new(["temp", ".txt"], "w+")

temp_file.puts "Dies ist eine temporäre Datei."
temp_file.close

puts File.read(temp_file.path)
```

Die Ausgabe dieses Codes sieht wie folgt aus:

![Beispiel Ausgabe](https://github.com/example.png)

## Tiefere Einblicke
Wenn du dich genauer für das Erstellen von temporären Dateien in Ruby interessierst, kannst du dir die verschiedenen Modi ansehen, in denen du eine tempfile-Instanz erstellen kannst. Zum Beispiel ist der Modus "w+" nicht der einzige verfügbare Modus. Du kannst auch "r+" verwenden, um eine Datei zum Lesen und Schreiben zu öffnen, oder "a+" um eine Datei zum Anhängen zu öffnen. Außerdem kannst du die tempfile-Instanz auch in einer Block-Syntax erstellen, die dafür sorgt, dass die temporäre Datei nach Beendigung des Blocks automatisch gelöscht wird.

## Siehe Auch
- [Ruby's tempfile library documentation](https://ruby-doc.org/stdlib-2.6.3/libdoc/tempfile/rdoc/Tempfile.html)
- [Creating Temporary Files in Ruby - Tutorial](https://blog.appsignal.com/2019/03/26/creating-temporary-files-in-ruby.html)
- [How to Work with Temp Files in Ruby](https://www.rubyguides.com/2015/04/temporary-files-ruby/)