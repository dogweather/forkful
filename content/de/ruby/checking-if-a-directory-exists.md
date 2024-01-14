---
title:                "Ruby: Überprüfung, ob ein Verzeichnis existiert"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

In der Welt der Programmierung gibt es viele verschiedene Aufgaben und Herausforderungen zu bewältigen. Eines der häufigsten Probleme ist das Überprüfen der Existenz eines Verzeichnisses. Aber warum sollte man das überhaupt tun?

Das Überprüfen, ob ein Verzeichnis existiert, ist besonders wichtig, wenn man mit Dateien und Ordnern interagiert, zum Beispiel beim Öffnen, Verschieben oder Löschen von Dateien. Wenn man nicht sicherstellt, dass das betreffende Verzeichnis existiert, kann dies zu Fehlern und unerwünschtem Verhalten führen. Daher ist es wichtig, diese grundlegende Aufgabe zu beherrschen, um sicherzustellen, dass das Programm reibungslos funktioniert.

## Wie geht das?

In Ruby gibt es eine eingebaute Methode namens `Dir.exist?`, die sehr nützlich ist, um die Existenz eines Verzeichnisses zu überprüfen. Hier ist ein kleines Beispiel, wie man diese Methode verwenden kann:

```Ruby
if Dir.exist?("Beispielverzeichnis")
    puts "Das Verzeichnis existiert."
else
    puts "Das Verzeichnis existiert nicht."
end
```

Das obige Beispiel wird zunächst überprüfen, ob das Verzeichnis "Beispielverzeichnis" existiert und je nach Ergebnis eine entsprechende Meldung ausgeben. Aber was passiert, wenn man auch den Pfad angeben möchte? Keine Sorge, auch dafür gibt es eine Lösung:

```Ruby
if Dir.exist?("/Users/Beispielverzeichnis/Test")
    puts "Das Verzeichnis existiert."
else
    puts "Das Verzeichnis existiert nicht."
end
```

Wie man sehen kann, gibt es auch die Möglichkeit, den vollständigen Pfad anzugeben. Die Methode erwartet als Argument entweder einen relativen oder absoluten Pfad.

## Tiefergehende Informationen

Wenn man genauer darüber nachdenkt, ist ein Verzeichnis eigentlich nur eine Art von Datei. Das lässt einen vielleicht glauben, dass man einfach die `File.exist?` Methode nutzen könnte, um die Existenz eines Verzeichnisses zu überprüfen. Aber das ist nicht unbedingt der Fall, da ein Verzeichnis einige Unterschiede zu anderen Dateien aufweist.

Wenn man also sicher sein möchte, dass es sich bei einem bestimmten Dateipfad auch um ein Verzeichnis handelt, sollte man die `Dir.exist?` Methode verwenden. Diese Methode überprüft genau, ob es sich um ein Verzeichnis handelt oder nicht.

## Siehe auch

* [Dir.exist? Dokumentation](https://ruby-doc.org/core-3.0.0/Dir.html#method-c-exist-3F)
* [Überprüfen der Existenz von Dateien und Verzeichnissen in Ruby](https://www.rubyguides.com/2017/09/ruby-file-exists/)
* [Was ist ein Verzeichnis? (auf Deutsch)](https://www.computerhope.com/jargon/d/webdir.htm)