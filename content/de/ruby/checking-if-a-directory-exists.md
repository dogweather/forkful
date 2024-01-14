---
title:    "Ruby: Überprüfen, ob ein Verzeichnis existiert"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Warum

Beim Programmieren kann es häufig vorkommen, dass man überprüfen muss, ob ein bestimmter Ordner oder eine Datei existiert. Dies kann hilfreich sein, um sicherzustellen, dass der Code reibungslos ausgeführt wird und um Fehler zu vermeiden. In diesem Blogbeitrag werden wir uns genauer ansehen, wie man in Ruby überprüfen kann, ob ein Verzeichnis existiert.

# Wie man überprüft, ob ein Verzeichnis existiert

Um zu überprüfen, ob ein Verzeichnis vorhanden ist, können wir die `File` Klasse in Ruby verwenden. Diese Klasse bietet verschiedene Methoden zur Überprüfung von Dateien und Verzeichnissen. Die Methode, die wir verwenden werden, ist die `directory?` Methode. Diese gibt `true` zurück, wenn der angegebene Pfad ein Verzeichnis ist, andernfalls wird `false` zurückgegeben. Um diese Methode zu verwenden, müssen wir den Pfad zu dem Verzeichnis als Argument übergeben.

```Ruby
if File.directory?("/Users/user/Downloads/")
  puts "Das Verzeichnis existiert."
else
  puts "Das Verzeichnis existiert nicht."
end
```

In diesem Beispiel überprüfen wir, ob das Verzeichnis "Downloads" im Benutzerordner existiert. Wenn das Verzeichnis vorhanden ist, wird "Das Verzeichnis existiert." ausgegeben, ansonsten wird "Das Verzeichnis existiert nicht." angezeigt.

# Tiefer Einblick

Wenn wir genauer verstehen möchten, wie die `directory?` Methode funktioniert, können wir uns das Quellcode ansehen. Dazu können wir die Methode `method` verwenden, um das Objekt der Methode zu erhalten, und dann die `source_location` Methode, um die Datei und Zeile zu erhalten, in der die Methode definiert ist.

```Ruby
puts File.method(:directory?).source_location
```

Die Ausgabe lautet: `["/usr/lib/ruby/2.6.0/file.rb", 2530]`. Wenn wir uns diese Datei ansehen, können wir sehen, dass die `directory?` Methode folgendermaßen definiert ist:

```Ruby
def directory?(file_name)
  lstat(file_name).directory?
end
```

Es wird also die `lstat` Methode verwendet, um Informationen über die Datei zu erhalten, und dann wird die `directory?` Methode auf diese Informationen angewendet, um zu überprüfen, ob es sich um ein Verzeichnis handelt. Das Verständnis des Quellcodes kann uns dabei helfen, die Funktionsweise von Ruby besser zu verstehen und unsere Programmierfähigkeiten zu verbessern.

# Siehe auch

- [Ruby - File Klasse](https://ruby-doc.org/core-2.6.6/File.html)
- [Ruby - lstat Methode](https://ruby-doc.org/core-2.6.6/FileStat.html#method-c-lstat)
- [Ruby - directory? Methode](https://ruby-doc.org/core-2.6.6/File/Stat.html#method-i-directory-3F)