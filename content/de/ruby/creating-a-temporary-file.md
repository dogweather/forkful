---
title:                "Erstellen einer temporären Datei"
html_title:           "Ruby: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Warum

Es gibt viele Gründe, warum man eine temporäre Datei in Ruby erstellen würde. Ein häufiger Anwendungsfall ist das Speichern von Zwischenergebnissen während der Ausführung eines Programms, um später darauf zugreifen zu können.

# Wie geht das?

Um eine temporäre Datei in Ruby zu erstellen, müssen wir zunächst die `Tempfile`-Klasse importieren. Dann können wir die `open`-Methode aufrufen und einen temporären Dateinamen angeben. Der Inhalt der Datei wird automatisch gelöscht, sobald das Programm beendet wird.

```Ruby
require 'tempfile' 

# Eine temporäre Datei mit dem Inhalt "Hello World" erstellen
temp_file = Tempfile.open('test_file')
temp_file.write("Hello World")

# Dateiinhalt lesen und ausgeben
puts temp_file.read # Output: Hello World

# Temporäre Datei schließen und löschen

temp_file.close
```

# Tief in die Materie eintauchen

Die `Tempfile`-Klasse bietet viele nützliche Methoden, um mit temporären Dateien umzugehen. Zum Beispiel können wir mit der `unlink`-Methode die Datei löschen, bevor sie geschlossen wird. Wir können auch den Pfad der temporären Datei mit der `path`-Methode abrufen.

```Ruby
# Temporäre Datei erstellen und Informationen abrufen 
temp_file = Tempfile.new('test')
puts temp_file.path # Output: /var/folders/gn/jlmh05tx4n127f8hd_dw1hkh0000gn/T/test20191214-3235-wp1fvi

# Datei löschen und überprüfen, dass sie nicht mehr existiert 
temp_file.unlink
puts File.exist?(temp_file.path) # Output: false
```

Ein weiterer nützlicher Aspekt der `Tempfile`-Klasse ist die Möglichkeit, den Dateipfad und den Namen anzupassen. Dies kann hilfreich sein, wenn wir verschiedene temporäre Dateien im selben Programm erstellen müssen.

```Ruby
# Temporäre Datei mit benutzerdefiniertem Namen erstellen 
temp_file = Tempfile.new(['custom_name', '.txt'])
puts temp_file.path # Output: /var/folders/gn/jlmh05tx4n127f8hd_dw1hkh0000gn/T/custom_name20191214-3235-bul5t0.txt 
```

# Siehe auch

- [Ruby Tempfile Dokumentation](https://ruby-doc.org/stdlib-2.6.5/libdoc/tempfile/rdoc/Tempfile.html)
- [Riost's Article "Temporary Files in Ruby"](https://riptutorial.com/ruby/example/2643/temporary-files-in-ruby)
- [Stack Overflow: "How to Create a Temporary File in Ruby"](https://stackoverflow.com/questions/16667445/how-to-create-a-temporary-file-in-ruby)