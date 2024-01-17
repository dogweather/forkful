---
title:                "Lesen von Befehlszeilenargumenten"
html_title:           "Ruby: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

### Was & Warum?
Das Lesen von Befehlszeilenargumenten ist eine Möglichkeit für Programmierer, benutzerdefinierte Eingaben von Benutzern zu akzeptieren und diese Eingaben in ihr Programm einzubinden. Es ist hilfreich, wenn ein Programm verschiedene Einstellungen oder Optionen bietet, die der Benutzer je nach Bedarf anpassen kann.

### Wie geht's?
Die Verwendung von Befehlszeilenargumenten in Ruby ist einfach und kann in wenigen Zeilen Code implementiert werden.

Beispielcode:
```
# Lese das erste Argument aus der Befehlszeile
arg1 = ARGV[0]

# Ausgabe des Arguments
puts "Das eingegebene Argument war: #{arg1}"
```

Beispieloutput:
```
$ ruby beispiel.rb Hallo
Das eingegebene Argument war: Hallo
```

### Tiefes Eintauchen
Das Lesen von Befehlszeilenargumenten ist eine weit verbreitete Praxis in der Programmierung und wird in verschiedenen Sprachen wie Ruby, Python und Java unterstützt. Eine alternative Möglichkeit, benutzerdefinierte Eingaben zu akzeptieren, ist die Verwendung von Umgebungsvariablen. Die Implementierung des Lesens von Befehlszeilenargumenten in Ruby basiert auf der globalen Variable `ARGV`, die eine Sammlung der Argumente, die beim Aufruf des Programms übergeben wurden, enthält.

### Sieh dir auch an
- [Ruby-Dokumentation zu Befehlszeilenargumenten](https://ruby-doc.org/core-2.7.0/ARGF.html)
- [Wie man Argumente aus der Befehlszeile in Python liest](https://docs.python.org/3/library/sys.html#sys.argv)
- [Reading Command-Line Arguments in Java](https://www.baeldung.com/java-command-line-arguments)