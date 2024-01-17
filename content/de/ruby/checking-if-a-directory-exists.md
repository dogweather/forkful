---
title:                "Überprüfung, ob ein Verzeichnis existiert"
html_title:           "Ruby: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?
 Prüfen, ob ein Verzeichnis existiert, ist eine wichtige Aufgabe für Programmierer, um sicherzustellen, dass ihr Programm erfolgreich ausgeführt werden kann. Es ist ein einfacher Weg, um sicherzustellen, dass die erforderlichen Verzeichnisse vorhanden sind, bevor man versucht, Dateien zu erstellen oder zu lesen.

## Wie geht's:
Um zu prüfen, ob ein Verzeichnis existiert, kann man die Methode ```Dir.exist?``` verwenden. Diese Methode gibt einen booleschen Wert zurück und kann direkt in bedingten Anweisungen verwendet werden. Zum Beispiel:

```Ruby
if Dir.exist?("Verzeichnis")
  puts "Ja, das Verzeichnis existiert."
else
  puts "Nein, das Verzeichnis existiert nicht."
end

## Deep Dive:
Historisch gesehen war die Überprüfung der Existenz von Verzeichnissen durch das Überprüfen der Rückgabe einer Dateioperation verbunden mit "File Specific Errors" die gängige Methode. Jedoch bringt diese Methode zwei Nachteile mit sich:

1. Sie ist anfällig für menschliches Versagen (z.B. Tippfehler bei der Pfadangabe).
2. Sie ist langsamer, da sie eine Dateioperation ausführen muss.

Die Verwendung von ```Dir.exist?``` ist eine einfachere und schnellere Alternative. Die Implementierung dieser Methode mit Ruby ist sehr effizient, da sie auf Systemaufrufe zurückgreift, die bereits vom Betriebssystem optimiert wurden.

## Siehe auch:
Weitere Informationen zum Prüfen der Existenz von Verzeichnissen in Ruby findest du in der [offiziellen Ruby-Dokumentation](https://ruby-doc.org/core-2.7.0/Dir.html#method-c-exist-3F).