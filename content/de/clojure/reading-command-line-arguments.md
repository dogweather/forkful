---
title:                "Das Lesen von Befehlszeilenargumenten"
html_title:           "Clojure: Das Lesen von Befehlszeilenargumenten"
simple_title:         "Das Lesen von Befehlszeilenargumenten"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Wenn du schon einmal eine Anwendung auf der Kommandozeile ausgeführt hast, hast du wahrscheinlich gesehen, dass sie manchmal zusätzliche Argumente erfordert. Diese Argumente ermöglichen es Benutzern, die Funktionalität der Anwendung anzupassen oder bestimmte Operationen auszuführen. In dieser Anleitung werden wir uns ansehen, wie man diese Argumente richtig liest und versteht.

## Wie geht's

Das Lesen von Befehlszeilenargumenten ist in Clojure ziemlich einfach. Alles, was du dafür brauchst, ist das ```clojure clojure.args``` Paket. Bevor wir jedoch loslegen, lass uns einen einfachen Beispielcode betrachten:

```clojure
(ns arg-example
  (:require [clojure.args :refer [parse-opts]]))

(defn main
  [args]
  (let [[opts _ _] (parse-opts args [["-n" "--name" "Name" "Name des Benutzers"]])]
    (println "Hallo" (:name opts))))

```

Dieses kurze Programm liest einen optionalen Befehlszeilenargument mit dem Namen "Name" und gibt "Hallo Name" aus, wobei "Name" der eingegebene Wert ist. Lass uns nun sehen, was passiert, wenn wir dieses Programm mit verschiedenen Argumenten ausführen:

```
$ java -jar arg-example.jar
Hallo

$ java -jar arg-example.jar -n "Mike"
Hallo Mike

$ java -jar arg-example.jar --name "Sandra"
Hallo Sandra
```

Wie du sehen kannst, kannst du das Argument entweder mit einer Kurzform (```-n```) oder einer Langform (```--name```) angeben, gefolgt von einem Wert. Wenn du das Argument nicht angibst, wird der Standardwert verwendet (in diesem Fall ist es ein leerer String).

Der Grund, warum dies so funktioniert, ist, dass das ```parse-opts``` Funktion ein Hashmap zurückgibt, das alle angegebenen Argumente enthält. In unserem Beispiel verwenden wir das ```_``` Symbol, um ungenutzte Argumente zu verwerfen, da wir hier nur an dem einen Argument interessiert sind. Du kannst jedoch beliebig viele Argumente hinzufügen.

## Tiefe Tauchgänge

Nun, da du gesehen hast, wie man Befehlszeilenargumente in Clojure liest, lass uns einen Blick auf die ```parse-opts``` Funktion werfen. Diese Funktion nimmt drei Argumente: die Argumente, die an dein Programm übergeben werden (normalerweise ```*command-line-args*```), eine Sequenz von zu parsenden Argumenten und eine optionale Kopfzeile für die Hilfe.

In unserem Beispiel haben wir nur ein Argument definiert, aber in der Sequenz kannst du beliebig viele Argumente angeben. Jede Argumentdefinition besteht aus einem oder mehreren Flaggen (wie in unserem "Name"-Argument), einem benutzerdefinierten Namen und einer Beschreibung für die Hilfe.

Die ```parse-opts``` Funktion sorgt dafür, dass die eingegebenen Argumente den erwarteten Formen entsprechen und gibt dann das Ergebnis als Hashmap zurück, das die benutzerdefinierten Namen als Schlüssel enthält. Es gibt auch die Möglichkeit, Standardwerte für Argumente anzugeben, falls der Benutzer sie nicht angegeben hat.

## Siehe auch

- [Offizielle Dokumentation für Clojure Args](https://clojure.github.io/tools.cli/)
- [Eine praktische Einführung in das Lesen von Befehlszeilenargumenten in Clojure](https://www.brainfuel.io/blog/how-to-read-command-line-arguments-with-clojure)