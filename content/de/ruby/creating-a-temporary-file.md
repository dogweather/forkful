---
title:                "Ruby: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum?

Wenn Sie Ruby-Code schreiben, müssen Sie möglicherweise temporäre Dateien erstellen. Dies kann hilfreich sein, um Daten zu speichern, auf die später zugegriffen werden muss, oder um Dateien zu nutzen, die von anderen Programmen erstellt wurden.

## Wie geht das?

Die Erstellung einer temporären Datei in Ruby ist einfach. Verwenden Sie die `Tempfile`-Klasse und rufen Sie `new` auf, um eine neue temporäre Datei zu erstellen.

````Ruby
require 'tempfile'

# Erstelle eine neue temporäre Datei
my_temp_file = Tempfile.new('beispiel')

# Schreibe in die Datei
my_temp_file.write('Dies ist ein Beispieltext.')

# Lese den Inhalt der Datei
puts my_temp_file.read

# Schließe die Datei
my_temp_file.close
````

Der Output des obigen Codes sollte `Dies ist ein Beispieltext.` sein. Beachten Sie, dass die temporäre Datei automatisch gelöscht wird, nachdem sie geschlossen wurde.

## Tiefer eintauchen

Die `Tempfile`-Klasse bietet verschiedene Methoden, die Ihnen bei der Verwendung von temporären Dateien helfen können. Zum Beispiel können Sie den Dateinamen mit `path` erhalten oder überprüfen, ob die Datei schon existiert mit `exist?`.

## Siehe auch

- [Die offizielle Ruby-Dokumentation zur Tempfile-Klasse](https://ruby-doc.org/stdlib-2.7.1/libdoc/tempfile/rdoc/Tempfile.html)
- [Ein Tutorial zur Verwendung von temporären Dateien in Ruby](https://www.rubyguides.com/2018/05/tempfile-ruby/)
- [Acht Dinge, die Sie über temporäre Dateien in Ruby wissen sollten](https://jasoncharnes.com/tempfile-ruby)