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

## Was & Warum?

Suchen und Ersetzen von Text ist ein häufig genutztes Werkzeug in der Programmierung. Es ermöglicht Programmierern, bestimmte Wörter oder Phrasen in einem Text zu identifizieren und durch andere zu ersetzen. Dies ist hilfreich, um wiederkehrende Abschnitte in Code schnell zu ändern oder Fehler zu beheben.

## Wie geht's?

Die Suchen-und-Ersetzen-Funktion ist in Clojure durch die Verwendung der integrierten Funktion `clojure.string/replace` verfügbar. Hier ein Beispiel, um alle Vorkommen von "Hallo" in einem String durch "Hi" zu ersetzen:

```
(clojure.string/replace "Hallo Welt" "Hallo" "Hi")
```

Dies ergibt den Output `"Hi Welt"`.

## Tief eintauchen

Historisch gesehen war das Suchen und Ersetzen von Text in früheren Programmiersprachen oft umständlicher, da es durch manuelles Iterieren durch die einzelnen Zeichen erfolgen musste. Heutzutage gibt es jedoch viele alternativen, wie z.B. das Verwenden von regulären Ausdrücken für eine präzisere Suche. Die Implementierung von Suchen und Ersetzen in Clojure basiert auf dem Konzept der persistenten Datenstrukturen, was die Effizienz und Performance verbessert.

## Siehe auch

- Offizielle Clojure-Dokumentation zur `clojure.string` Bibliothek: https://clojuredocs.org/clojure.string
- Vergleich von Suchen-und-Ersetzen-Funktionen in verschiedenen Programmiersprachen: https://www.regular-expressions.info/repl.html