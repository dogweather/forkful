---
title:    "Clojure: Lesen von Befehlszeilenargumenten"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Warum

Das Lesen von Befehlszeilenargumenten ist eine wichtige Fähigkeit für jeden Programmierer, der mit der Entwicklung von Anwendungen in Clojure arbeitet. Es ermöglicht Ihnen, eine interaktive Schnittstelle mit Ihrer Anwendung zu schaffen und Benutzereingaben effektiv zu verarbeiten.

# Wie geht man vor?

Um Befehlszeilenargumente in Clojure zu lesen, können Sie die in der Standardbibliothek enthaltene Funktion `command-line-args` verwenden. Diese Funktion gibt eine Liste der Befehlszeilenargumente zurück, die dem Aufruf Ihrer Anwendung übergeben wurden.

Lassen Sie uns anhand eines Beispiels sehen, wie diese Funktion funktioniert:

```Clojure
(defn print-args []
  (println "Die übergebenen Argumente sind:" (str (command-line-args))))

(print-args)
```

Wenn wir nun unsere Datei mit dem Befehl `clojure datei.clj eins zwei drei` ausführen, erhalten wir die folgende Ausgabe:

```
Die übergebenen Argumente sind: (eins zwei drei)
```

# Tiefergehende Erläuterung

Die `command-line-args` Funktion ist sehr flexibel und kann mit optionalen Argumenten angepasst werden. Wenn Sie beispielsweise nur die nicht optionale Argumente zurückgeben möchten, können Sie die `:require-args` Option verwenden.

```Clojure
(defn print-args []
  (println "Nicht optionale Argumente:" (str (command-line-args :require-args))))

(print-args)
```

Wenn wir nun unsere Datei mit dem Befehl `clojure datei.clj -a eins -b zwei` ausführen, erhalten wir die folgende Ausgabe:

```
Nicht optionale Argumente: (eins zwei)
```

Sie können auch die `:as` Option verwenden, um die zurückgegebene Liste in einen Hashmap mit Schlüssel-Wert-Paaren umzuwandeln.

```Clojure
(defn print-args []
  (println "Argumente als Hashmap:" (str (command-line-args :as :options))))

(print-args)
```

Wenn wir nun unsere Datei mit dem Befehl `clojure datei.clj -a eins -b zwei` ausführen, erhalten wir die folgende Ausgabe:

```
Argumente als Hashmap: {:a "eins", :b "zwei"}
```

# Siehe auch

- Clojure Standardbibliothek Dokumentation für `command-line-args`: https://clojuredocs.org/clojure.core/command-line-args
- Ein ausführliches Tutorial zum Lesen von Befehlszeilenargumenten in Clojure: https://www.tutorialspoint.com/clojure/clojure_command_line_arguments.htm
- Eine Diskussion über die Verwendung von Befehlszeilenargumenten in Clojure: https://stackoverflow.com/questions/18547922/how-do-i-read-command-line-arguments-in-clojure