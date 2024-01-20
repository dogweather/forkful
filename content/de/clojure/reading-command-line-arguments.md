---
title:                "Befehlszeilenargumente lesen"
html_title:           "Arduino: Befehlszeilenargumente lesen"
simple_title:         "Befehlszeilenargumente lesen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was und Warum?

Beim Lesen von Befehlszeilenargumenten greifen wir auf die Parameter zu, die an ein Programm beim Start übergeben wurden. Es ermöglicht unseren Programmen, flexibel und dynamisch in Bezug auf ihre Konfiguration und Verwendung zu sein.

## So geht's:

In Clojure verwenden wir die `*command-line-args*` Variable zur Verarbeitung von Befehlszeilenargumenten. Hier ein einfaches Beispiel:

```Clojure
(defn -main
  [& args]
  (println "Command line arguments: " args))
```

Wenn Sie das Programm mit `lein run Hello World` ausführen, würden Sie folgende Ausgabe erhalten:

```Clojure
Command line arguments:  (Hello World)
```

## Vertiefung

Befehlszeilenargumente waren eine Methode zur Datenkommunikation mit Programmen seit den frühesten Tagen der Computernutzung.

Alternativ könnten Sie Möglichkeiten überprüfen, wie Java`s 'public static void main(String[] args)' oder Python`s 'sys.argv' in Clojure verwendet werden können. Dies könnte aufwendiger sein, da Sie die Java Interoperabilität oder Jython verwenden müssten.

Die `*command-line-args*` Variable ist ein dynamischer Var in Clojure, die eine Liste aller Argumente enthält, die ihr beim Start übergeben wurden.

## Siehe auch

1. Clojure-Dokumentation: [Command Line Arguments](https://clojure.org/guides/deps_and_cli)
2. Stack Overflow: [Passing command-line arguments in Clojure](https://stackoverflow.com/questions/2977898/passing-command-line-arguments-in-clojure)

Jetzt haben Sie eine praktische Möglichkeit zu verstehen, wie Sie Befehlszeilenargumente in Ihrem Clojure-Programm verwenden können. Frohes Codieren!