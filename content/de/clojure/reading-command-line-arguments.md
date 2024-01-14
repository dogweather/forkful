---
title:    "Clojure: Erfassen von Befehlszeilenargumenten"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Warum

Das Lesen von Kommandozeilenargumenten ist ein wesentlicher Bestandteil der Programmierung in Clojure. Mit dieser Fähigkeit können wir unsere Programme anpassen und sie flexibler gestalten. In diesem Artikel zeigen wir Ihnen, wie Sie Kommandozeilenargumente in Clojure lesen können und welche Vorteile dies mit sich bringt.

## Wie geht's?

Um Kommandozeilenargumente in Clojure zu lesen, verwenden wir die built-in Funktion `clojure.main`. Diese Funktion akzeptiert die eingebauten Kommandozeilenargumente und speichert sie in einer Liste, auf die wir dann zugreifen können.

```Clojure

(defn -main [& args]
  (println "Eingegebene Argumente:")
  (doseq [arg args]
    (println arg)))
```

Die Funktion `defn` definiert eine Funktion mit dem Namen `-main`, die eine Variable `args` aufnimmt. In unserem Beispiel verwenden wir die Funktion `doseq`, um durch die Liste der Argumente zu iterieren und jedes einzelne auf der Konsole auszugeben.

Wenn wir nun unser Programm mit einigen Argumenten ausführen, z.B. `clojure -m my-program arg1 arg2`, wird die Ausgabe folgendermaßen aussehen:

```
Eingegebene Argumente:
arg1
arg2
```

## Tiefer einsteigen

Um ein besseres Verständnis dafür zu bekommen, wie Kommandozeilenargumente in Clojure funktionieren, müssen wir etwas tiefer eintauchen. Mit der Funktion `clojure.core/command-line-args` können wir die eingebauten Argumente in einer Liste von Strings speichern. Diese Funktion ist nützlich, wenn wir die Argumente weiterverarbeiten möchten, z.B. als Eingabeparameter für eine Funktion.

```Clojure
(defn -main []
  (let [args (clojure.core/command-line-args)]
    (println "Erstes Argument:" (first args))
    (println "Zweites Argument: (second args)")))
```

Wenn wir dieses Programm ausführen, werden wir feststellen, dass es dieselbe Ausgabe wie zuvor erzeugt. Je nachdem, was für ein Programm Sie schreiben, können Sie die Kommandozeilenargumente jetzt verwenden, um bestimmte Aktionen auszuführen oder unterschiedliche Ausgaben zu erzeugen.

## Siehe auch

Hier sind einige andere Ressourcen, die Sie sich ansehen können, um mehr über das Lesen von Kommandozeilenargumenten in Clojure zu erfahren:

- [Offizielle Dokumentation der Funktion `clojure.core/command-line-args`](https://clojuredocs.org/clojure.core/command-line-args)
- [Artikel von David Nolen über die Verwendung von Kommandozeilenargumenten in Clojure](https://cemerick.com/2008/09/03/command-line-arguments-in-clojure/)
- [Stack Overflow Diskussion über die Verarbeitung von Kommandozeilenargumenten in Clojure](https://stackoverflow.com/questions/6079578/how-can-i-pass-command-line-arguments-to-a-clojure-program/6079913#6079913)