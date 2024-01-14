---
title:    "Ruby: Überprüfung, ob ein Verzeichnis existiert"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Warum

Stellen Sie sich vor, Sie schreiben ein Ruby-Programm, das auf Dateien und Verzeichnisse zugreift. Um sicherzustellen, dass Ihr Programm reibungslos funktioniert, möchten Sie prüfen, ob ein bestimmtes Verzeichnis existiert, bevor Sie versuchen, darauf zuzugreifen. Dies ist der Grund, warum das Überprüfen der Existenz eines Verzeichnisses eine wichtige Aufgabe in der Ruby-Programmierung ist.

## Wie

Um zu überprüfen, ob ein Verzeichnis in Ruby existiert, können Sie die `Dir.exist?` Methode verwenden. Diese Methode nimmt den Pfad des zu überprüfenden Verzeichnisses als Argument und gibt `true` zurück, wenn das Verzeichnis existiert, andernfalls `false`.

```Ruby
if Dir.exist?("Pfad/zum/Verzeichnis")
  puts "Das Verzeichnis existiert."
else
  puts "Das Verzeichnis existiert nicht."
end
```

Die Ausgabe hängt davon ab, ob das Verzeichnis existiert oder nicht:

```
Das Verzeichnis existiert.
```

oder

```
Das Verzeichnis existiert nicht.
```

## Vertiefung

Die `Dir.exist?` Methode prüft nur, ob das Verzeichnis selbst existiert. Um sicherzustellen, dass auch alle Unterverzeichnisse und Dateien innerhalb des Verzeichnisses vorhanden sind, können Sie die `Dir.entries` Methode verwenden. Diese Methode gibt eine Liste mit den Namen aller Dateien und Unterverzeichnisse innerhalb des angegebenen Verzeichnisses zurück.

```Ruby
if Dir.exist?("Pfad/zum/Verzeichnis")
  puts "Das Verzeichnis existiert."
  puts "Inhalt des Verzeichnisses:"
  puts Dir.entries("Pfad/zum/Verzeichnis")
else
  puts "Das Verzeichnis existiert nicht."
end
```

Die Ausgabe sieht dann beispielsweise wie folgt aus:

```
Das Verzeichnis existiert.
Inhalt des Verzeichnisses:
.
..
datei1.txt
datei2.txt
subverzeichnis
```

## Siehe auch

- [Offizielle Dokumentation zu `Dir.exist?`](https://ruby-doc.org/core/Dir.html#method-c-exist-3F)
- [Weitere Verzeichnis-Methoden in Ruby](https://www.rubyguides.com/2015/07/ruby-dir-children/) 
- [Beispiele für das Arbeiten mit Verzeichnissen in Ruby](https://www.tutorialspoint.com/ruby/ruby_directory_operations.htm)