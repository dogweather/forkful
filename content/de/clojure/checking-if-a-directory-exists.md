---
title:                "Clojure: Überprüfung, ob ein Verzeichnis vorhanden ist"
simple_title:         "Überprüfung, ob ein Verzeichnis vorhanden ist"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Warum

Ob Sie ein erfahrener Clojure-Entwickler oder ein Neuling in der Programmiersprache sind, Sie wissen wahrscheinlich, dass das Überprüfen der Existenz eines Verzeichnisses ein wichtiger Teil der Dateiverwaltung ist. In diesem Blog-Beitrag werden wir uns genauer damit befassen, wie man in Clojure überprüft, ob ein Verzeichnis existiert.

# Wie geht das?

Das Überprüfen der Existenz eines Verzeichnisses ist ein relativ einfacher Prozess in Clojure. Wir verwenden die Funktion `clojure.java.io/file` und die Funktion `(.isDirectory f)`, um die Existenz des Verzeichnisses zu überprüfen. Schauen wir uns ein Beispiel an:

```Clojure
(let [f (clojure.java.io/file "/home/user/documents")]
  (.isDirectory f))
```

Das obige Beispiel wird `true` zurückgeben, wenn das Verzeichnis `/home/user/documents` existiert, und `false`, wenn es nicht existiert.

Wir können auch eine Funktion schreiben, die uns die Anzahl der Dateien in einem Verzeichnis zurückgibt:

```Clojure
(defn file-count [dir]
  (let [files (clojure.java.io/file dir)
        files-count (count (.listFiles files))]
    (println (str "Das Verzeichnis " files " enthält " files-count " Dateien."))))

(file-count "/home/user/documents")
```

Die Ausgabe dieses Codes wird wie folgt aussehen:

```bash
Das Verzeichnis /home/user/documents enthält 5 Dateien.
```

# Tiefergehende Informationen

Wenn Sie sich für die Funktionsweise der Funktionen `clojure.java.io/file` und `(.isDirectory f)` interessieren, können Sie sich die Dokumentation dazu ansehen. Sie können auch mit anderen Funktionen wie `(.listFiles f)` und `(.getName f)` experimentieren, um mehr über Dateiverwaltung in Clojure zu lernen.

# Siehe auch

- [Clojure Dokumentation für Dateiverwaltung](https://clojure.github.io/clojure/clojure.java.io-api.html)
- [Eine Einführung in Clojure für Anfänger](https://medium.com/swlh/an-introduction-to-clojure-for-beginners-23ff7487880d)
- [Clojure-Programmierung auf Deutsch lernen](https://www.clojure.org/community/online-resources#deutsch)

Vielen Dank fürs Lesen! Wir hoffen, dass Sie jetzt ein besseres Verständnis davon haben, wie man in Clojure überprüft, ob ein Verzeichnis existiert. Happy coding!