---
title:                "Ausgabe von Debug-Informationen"
html_title:           "Ruby: Ausgabe von Debug-Informationen"
simple_title:         "Ausgabe von Debug-Informationen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum
Druckausgabe ist ein nützliches Werkzeug für Ruby-Entwickler, um Probleme und Fehler in ihrem Code zu identifizieren und zu beheben. Durch die Ausgabe von Debug-Informationen können Entwickler den Ablauf ihres Programms besser verstehen und mögliche Ursachen für Fehler leichter finden.

## Wie geht man vor
Der einfachste Weg, Debug-Informationen in Ruby auszugeben, ist die Verwendung der `puts` Methode. Diese Methode nimmt eine oder mehrere Variablen als Argumente und gibt sie in der Konsole aus. Zum Beispiel:

```Ruby
puts "Hallo Welt!"
# Ausgabe: Hallo Welt!
```

Debug-Informationen können auch in komplexeren Szenarien verwendet werden, z.B. um den Wert von Variablen in einer Schleife zu überwachen. Zum Beispiel:

```Ruby
5.times do |i|
  puts "Der Wert von i ist: #{i}"
end
# Ausgabe:
# Der Wert von i ist: 0
# Der Wert von i ist: 1
# Der Wert von i ist: 2
# Der Wert von i ist: 3
# Der Wert von i ist: 4
```

Eine weitere Methode zur Ausgabe von Debug-Informationen ist die `p` Methode. Im Gegensatz zu `puts` gibt `p` nicht nur den Wert einer Variable, sondern auch deren Datenstruktur aus. Zum Beispiel:

```Ruby
arr = [1, 2, 3]
puts arr
# Ausgabe: 1, 2, 3
p arr
# Ausgabe: [1, 2, 3]
```

## Tiefergehende Information
Es gibt auch einige Optionen, die die Ausgabe von Debug-Informationen in Ruby mächtiger machen. Zum Beispiel ermöglicht die `inspect` Methode, die auf ein beliebiges Objekt angewendet werden kann, eine detaillierte Darstellung seiner Eigenschaften. Zum Beispiel:

```Ruby
arr = [1, 2, 3]
puts arr.inspect
# Ausgabe: "[1, 2, 3]"
```

Eine weitere nützliche Option ist die Verwendung von `Logger`, einem Ruby-Modul, das speziell für die Verwaltung von Protokollen und Debug-Informationen entwickelt wurde. Mit `Logger` können Entwickler Ausgaben in verschiedene Dateien schreiben und die Log-Ebene anpassen, um nur relevante Informationen anzuzeigen.

## Siehe auch
- [Official Ruby Documentation on Puts](https://ruby-doc.org/core-#{version}/IO.html#method-i-puts )
- [Official Ruby Documentation on Logging](https://ruby-doc.org/stdlib-#{version}.8/libdoc/logger/rdoc/Logger.html)