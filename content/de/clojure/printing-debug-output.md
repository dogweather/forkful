---
title:    "Clojure: Druck von Debug-Ausgabe"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Das Drucken von Debug-Ausgaben während der Programmierung ist eine einfache und effektive Möglichkeit, um Fehler in Ihrem Code zu finden und zu beheben. Es ermöglicht es Ihnen, den genauen Ablauf Ihres Programms zu überprüfen und potenzielle Probleme zu identifizieren. 

## Wie das geht

Um Debug-Ausgaben in Clojure zu drucken, können Sie die `println` Funktion verwenden. Diese Funktion akzeptiert eine beliebige Anzahl von Argumenten und druckt sie in der Konsole aus. Hier ist ein Beispiel:

```Clojure
(println "Debug-Ausgabe" 42)
```

Dies wird `Debug-Ausgabe 42` in der Konsole ausgeben. Sie können auch Variablen oder Ausdrücke in `println` verwenden, um deren Werte zu überprüfen. 

Eine andere Möglichkeit, Debug-Ausgaben zu drucken, ist die Verwendung der `clojure.pprint` Bibliothek. Diese Bibliothek bietet mehrere Funktionen, mit denen Sie das Format und die Darstellung Ihrer Ausgabe anpassen können. Hier ist ein Beispiel:

```Clojure
(require '[clojure.pprint :refer [pprint]])

(pprint {:name "John" :age 27})
```

Dies wird ein formatiertes Output mit dem Schlüssel `:name` und Wert `"John"` und dem Schlüssel `:age` und Wert `27` ausgeben. 

## Tiefer schürfen

Das Drucken von Debug-Ausgaben kann auch nützlich sein, um zu verstehen, wie Ihre Funktionen und Makros arbeiten. Durch das Hinzufügen von Debug-Ausgaben an verschiedenen Stellen in Ihrem Code können Sie den Ablauf verfolgen und mögliche Probleme wie unerwartete Werte oder Schleifen erkennen. 

Sie können auch verschiedene Level von Debug-Ausgaben verwenden, um zwischen grundlegenden Informationen und detaillierten Einblicken zu unterscheiden. Dies kann hilfreich sein, um nur wichtige Ausgaben anzuzeigen und die Menge an Output zu reduzieren.

## Siehe auch

- [clojure.pprint Dokumentation](https://clojuredocs.org/clojure.pprint)
- [Clojure für Anfänger](https://learnxinyminutes.com/docs/clojure/) 
- [Web development mit Clojure](https://gettingclojure.com/)