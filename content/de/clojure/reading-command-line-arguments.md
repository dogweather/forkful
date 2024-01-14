---
title:                "Clojure: Das Lesen von Befehlszeilenargumenten"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Warum

Das Lesen von Befehlszeilenargumenten ist eine wichtige Fähigkeit für jeden, der mit der Programmiersprache Clojure arbeitet. Es ermöglicht das interaktive und flexible Einlesen von Daten in ein Programm und ermöglicht es, verschiedene Funktionen des Programms je nach den eingegebenen Argumenten auszuführen. In diesem Artikel werden wir uns damit beschäftigen, wie man in Clojure Befehlszeilenargumente liest und wie man sie in seinen Code integriert.

# Wie geht man vor?

In Clojure gibt es eine spezielle Funktion namens `command-line-args`, die verwendet wird, um die Befehlszeilenargumente auszulesen. Diese Funktion gibt eine Liste von Strings zurück, die die eingegebenen Argumente darstellen. Hier ist ein Beispiel:

```Clojure
(def args (command-line-args))
```

Das oben genannte Code-Beispiel liest die Befehlszeilenargumente in die Variable `args` ein. Um nun auf ein bestimmtes Argument zuzugreifen, können wir einfach den Index des gewünschten Arguments in der Liste angeben. Zum Beispiel, um das erste Argument zu erhalten, würden wir `args[0]` schreiben.

# Tiefergehende Analyse

Der `command-line-args` Funktion kann auch ein optionaler Parameter übergeben werden, der bestimmt, wie die Argumente interpretiert werden sollen. Standardmäßig wird angenommen, dass die Argumente durch Leerzeichen getrennt sind. Wenn jedoch ein `-m` Parameter übergeben wird, wird angenommen, dass die Argumente durch Kommata getrennt sind.

Eine weitere nützliche Funktion ist `apply`, die eine Funktion auf eine Liste von Argumenten anwendet. Nehmen wir zum Beispiel an, wir haben eine Funktion `print-args`, die die Befehlszeilenargumente in der Reihenfolge, in der sie eingegeben wurden, ausdrucken soll. Wir können dies wie folgt erreichen:

```Clojure
(defn print-args [args]
  (doseq [arg args]
    (println arg)))

(apply print-args args)
```

In diesem Beispiel wird die `apply` Funktion verwendet, um die `print-args` Funktion auf die `args` Liste anzuwenden.

# Siehe auch

- [Offizielle Dokumentation von Clojure zu Befehlszeilenargumenten](https://clojuredocs.org/clojure.core/command-line-args)
- [Clojure Befehlszeilenargumente Tutorial von TutorialsPoint](https://www.tutorialspoint.com/clojure/clojure_command_line_arguments.htm)
- [Clojure Cheat Sheet mit nützlichen Funktionen, einschließlich `command-line-args` und `apply`](https://clojure.org/api/cheatsheet)

Vielen Dank für das Lesen dieses Artikels! Jetzt wissen Sie, wie Sie Befehlszeilenargumente in Ihren Clojure Code einbinden können. Wir hoffen, dass das in Ihren zukünftigen Projekten nützlich sein wird. Bis zum nächsten Mal!