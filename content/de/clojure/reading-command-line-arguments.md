---
title:                "Clojure: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Befehlszeilenargumenten ist ein wichtiger Bestandteil der Programmierung in Clojure. Es ermöglicht es uns, Interaktion mit unserem Programm auf einer höheren Ebene zu haben und es an verschiedene Umgebungen anzupassen. In diesem Blogbeitrag werden wir uns genauer ansehen, wie man in Clojure Befehlszeilenargumente liest und verwendet.

## Anleitung

Um Befehlszeilenargumente in Clojure zu lesen, verwenden wir die `command-line-args`-Funktion. Diese Funktion gibt uns eine Liste der Befehlszeilenargumente zurück, die beim Start des Programms angegeben wurden. Schauen wir uns ein Beispiel an:

```Clojure
(def args (command-line-args))

(println (str "Die Befehlszeilenargumente waren: " args))
```

Wenn wir dieses Programm mit `clojure -M -m meinprogramm arg1 arg2` aufrufen, wird die Ausgabe folgendermaßen aussehen:

```
Die Befehlszeilenargumente waren: [arg1 arg2]
```

Wir können auch die `get`-Funktion verwenden, um auf bestimmte Argumente zuzugreifen, indem wir ihre Position in der Liste angeben. Zum Beispiel:

```Clojure
(def first-arg (get args 0))

(println (str "Das erste Argument war: " first-arg))
```

Die Ausgabe wäre:

```
Das erste Argument war: arg1
```

## Tieferer Einblick

Es ist wichtig zu beachten, dass Befehlszeilenargumente standardmäßig als Zeichenfolgen interpretiert werden. Wenn wir also eine Zahl als Argument übergeben, müssen wir sie zuerst in ein numerisches Format konvertieren, bevor wir sie verwenden können. Wir können dies mit der `read-string`-Funktion tun. Zum Beispiel:

```Clojure
(def num-arg (read-string (get args 0)))

(println (str "Die summe von " num-arg " und 5 ist: " (+ num-arg 5)))
```

Wenn wir das Programm mit `clojure -M -m meinprogramm 10` aufrufen, wird die Ausgabe folgendermaßen aussehen:

```
Die Summe von 10 und 5 ist: 15
```

Es ist auch möglich, optionale Argumente zu definieren, indem man vor dem Argument einen Schrägstrich setzt. Diese Argumente müssen nicht bei jedem Aufruf des Programms angegeben werden und werden standardmäßig auf `nil` gesetzt. Zum Beispiel:

```Clojure
(println (str "Optional 'foo' argument: " (get args 1)))
```

Wenn wir das Programm mit `clojure -M -m meinprogramm arg1` aufrufen, wird die Ausgabe folgendermaßen aussehen:

```
Optional 'foo' argument: nil
```

## Siehe auch

- [Clojure Dokumentation zu Befehlszeilenargumenten](https://clojure.org/reference/compilation)
- [Tutorial zur Verwendung von Befehlszeilenargumenten in Clojure](https://www.braveclojure.com/sequences-in-clojure/)
- [Clojure Cheatsheet für Befehlszeilenargumente](https://clojure.org/api/cheatsheet)
- [StackOverflow-Frage zur Verwendung von Befehlszeilenargumenten in Clojure](https://stackoverflow.com/questions/38999891/how-to-pass-command-line-arguments-using-clojure-script)