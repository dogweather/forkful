---
title:                "Clojure: Überprüfen, ob ein Verzeichnis vorhanden ist"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

Warum: 
Warum sollte man überprüfen, ob ein Verzeichnis existiert? Eine der Hauptgründe ist die Vermeidung von Fehlern bei der Ausführung von Code, der auf Verzeichnisse zugreift. Durch die Überprüfung auf die Existenz eines Verzeichnisses können unerwünschte Abstürze oder Fehlermeldungen vermieden werden.

Wie: Um ein Verzeichnis auf seine Existenz zu prüfen, gibt es in Clojure verschiedene Möglichkeiten. Eine einfache Möglichkeit ist die Verwendung der Funktion "dir-exists?", die als Argument den Pfad des zu überprüfenden Verzeichnisses annimmt. 

```Clojure
(if (dir-exists? "Pfad/zum/Verzeichnis")
  (println "Das Verzeichnis existiert.")
  (println "Das Verzeichnis existiert nicht."))
```

Die Ausgabe dieses Codes könnte wie folgt aussehen:

```
Das Verzeichnis existiert.
```

Um sicherzustellen, dass es sich wirklich um ein Verzeichnis und nicht um eine Datei handelt, kann die Funktion "dir?" verwendet werden.

```Clojure
(if (and (dir-exists? "Pfad/zum/Verzeichnis") (dir? "Pfad/zum/Verzeichnis"))
  (println "Es handelt sich um ein Verzeichnis.")
  (println "Es handelt sich nicht um ein Verzeichnis."))
```

Die Ausgabe wäre:

```
Es handelt sich um ein Verzeichnis.
```

Tieferer Einblick: Beim Überprüfen von Verzeichnissen gibt es einige Dinge zu beachten. Zum Beispiel kann es vorkommen, dass ein Verzeichnis gelöscht wird, während ein Programm darauf zugreift. In diesem Fall kann es nützlich sein, einen Mechanismus zu implementieren, um den Zugriff auf das Verzeichnis in solchen Fällen zu verhindern. Auch die Verwendung von absoluten oder relativen Pfaden kann Auswirkungen auf die Überprüfung haben.

Ein weiterer Faktor ist die Plattformunabhängigkeit. Clojure bietet die Funktion "file-separator" an, um das richtige Trennzeichen für Pfadangaben je nach Betriebssystem zu ermitteln. Dies kann bei der Überprüfung von Verzeichnissen hilfreich sein, da das Trennzeichen in den Pfaden variieren kann.

Siehe auch: 

- [Clojure Dokumentation für Verzeichnisfunktionen](https://clojure.github.io/clojure/clojure.java.io-api.html)
- [Blog-Post über Datei- und Verzeichnisoperationen in Clojure](https://www.rockyourcode.com/clojure-create-directory-check-if-directory-exists/) 
- [Clojure Cheatsheet für Datei- und Verzeichnisoperationen](https://clojuredocs.org/quickref#_file_and_directory_functions)