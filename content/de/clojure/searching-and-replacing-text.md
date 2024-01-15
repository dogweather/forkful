---
title:                "Suchen und Ersetzen von Text"
html_title:           "Clojure: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Das Suchen und Ersetzen von Text ist eine häufige Aufgabe beim Programmieren. Es ermöglicht uns, Teile von Text zu identifizieren und zu ersetzen, um unser Code schneller und effizienter zu machen.

## How To

Um Text in einer Clojure-Anwendung zu suchen und zu ersetzen, können wir die Funktionen `replace` und `replace-first` verwenden. Diese Funktionen nehmen einen regulären Ausdruck und die zu ersetzenden Werte als Argumente. Hier ist ein Beispiel, wie wir alle Vorkommen von "Hallo" durch "Guten Tag" ersetzen können:

```Clojure
(replace #"Hallo" "Guten Tag" "Hallo Welt") ; Ausgabe: "Guten Tag Welt"
```

Um nur das erste Vorkommen von "Hallo" zu ersetzen, können wir `replace-first` verwenden:

```Clojure
(replace-first #"Hallo" "Guten Tag" "Hallo Welt") ; Ausgabe: "Guten Tag Welt"
```

Beachten Sie, dass der reguläre Ausdruck immer als Pattern-Objekt übergeben werden muss, daher verwenden wir den `#`-Operator, um einen regulären Ausdruck zu kennzeichnen.

## Deep Dive

In Clojure können wir auch mithilfe von `loop` und `recur` eine rekursive Funktion erstellen, um Text zu suchen und zu ersetzen. Hier ist eine Beispielimplementation:

```Clojure
(defn ersetzen [text keyword newValue]
  (loop [result text]
    (if (re-find keyword result)
      (recur (clojure.string/replace-first result keyword newValue))
      result)))
```

In dieser Funktion wird mittels `loop` und `recur` immer wieder die Funktion aufgerufen, bis alle Vorkommen von `keyword` in `text` gefunden und ersetzt wurden.

## Siehe auch

- Dokumentation zu `replace` und `replace-first`: https://clojuredocs.org/clojure.string/replace
- Reguläre Ausdrücke in Clojure: https://clojuredocs.org/clojure.core/regex