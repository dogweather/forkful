---
title:                "Clojure: Debug-Ausgabe drucken"
programming_language: "Clojure"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

# Warum

Wahrscheinlich gibt es kaum einen Programmierer da draußen, der noch nicht debuggen musste. Und eine der nützlichsten Techniken beim Debuggen ist das Ausgeben von Debug-Informationen. Doch warum ist das eigentlich so? Nun, Debug-Output ermöglicht es uns, den Programmablauf zu verfolgen und zu verstehen, welche Werte an bestimmten Stellen im Code verwendet werden. Es ist eine effektive Methode, um Fehler in unserem Code zu finden und zu beheben.

# Wie

Um Debug-Informationen auszugeben, können wir die "println" Funktion in Clojure verwenden. Diese Funktion ermöglicht es uns, beliebige Werte auszugeben, indem wir sie als Parameter übergeben. Schauen wir uns dazu ein Beispiel an:

```Clojure
(def x 5)
(println "Der Wert von x ist:" x)
```

Die Ausgabe dieses Codes wäre "Der Wert von x ist: 5". Wie wir sehen können, können wir nicht nur konkrete Werte, sondern auch Variablen ausgeben. Das ist besonders hilfreich, um den tatsächlichen Wert einer Variable zu überprüfen, wenn wir den Verdacht haben, dass dort ein Fehler vorliegt. Wir können auch mehrere Werte ausgeben, indem wir sie mit einem Leerzeichen trennen, z.B. `(println x "ist größer als" y)`.

# Deep Dive

Nun, da wir wissen, wie man Debug-Output in Clojure verwendet, wollen wir uns noch genauer damit beschäftigen. Es gibt ein paar Tipps und Tricks, die uns helfen können, noch effektiver zu debuggen. Zum Beispiel können wir die "pr" Funktion verwenden, um auch die Datenstruktur unseres Werts auszugeben. Außerdem können wir die "with-out-str" Funktion verwenden, um die Ausgabe in eine Variable zu speichern, anstatt sie auf der Konsole auszugeben. Und wenn wir nur Debug-Output für bestimmte Teile unseres Codes möchten, können wir die "when" oder "if" Funktion verwenden, um die Ausgabebedingung zu bestimmen.

# Siehe auch

- https://clojuredocs.org/clojure.core/println
- https://clojuredocs.org/clojure.core/pr
- https://clojuredocs.org/clojure.core/with-out-str
- https://clojuredocs.org/clojure.core/when
- https://clojuredocs.org/clojure.core/if