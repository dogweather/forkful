---
title:                "Verknüpfung von Zeichenketten"
html_title:           "Clojure: Verknüpfung von Zeichenketten"
simple_title:         "Verknüpfung von Zeichenketten"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Das Verketten von Zeichenfolgen ist eine häufig genutzte Funktion in der Programmierung, die es ermöglicht, mehrere Zeichenfolgen miteinander zu kombinieren, um eine längere Zeichenfolge zu erstellen. Dies kann nützlich sein, um zum Beispiel Texte dynamisch zu generieren oder Daten in einem bestimmten Format auszugeben.

## How To

Um Zeichenfolgen in Clojure zu verketten, verwenden wir die Funktion `str`. Diese nimmt beliebig viele Argumente entgegen und gibt eine neue Zeichenfolge zurück, die aus allen Argumenten zusammengesetzt wird. Schauen wir uns dazu ein paar Beispiele an:

```Clojure
(str "Hallo" " " "Welt") ;; Ausgabe: Hallo Welt

(str "Mein Alter ist" 25) ;; Ausgabe: Mein Alter ist 25

(str "Das Ergebnis von" 5 "+" 3 "ist" (+ 5 3)) ;; Ausgabe: Das Ergebnis von 5 + 3 ist 8
```

In diesen Beispielen können wir sehen, dass `str` nicht nur Zeichenfolgen, sondern auch andere Datentypen wie Zahlen akzeptiert. Es verbindet alle Argumente zu einer neuen Zeichenfolge und gibt sie aus.

## Deep Dive

Die `str` Funktion verwendet intern die Funktion `StringBuilder`, um die Argumente zu verketten. Dadurch ist sie viel effizienter als eine naive Implementierung, bei der jede Zeichenfolge einzeln aneinandergehängt wird. Dies ist besonders wichtig, wenn wir mit großen Zeichenfolgen arbeiten, da es die Leistung unserer Code deutlich verbessern kann.

Es ist auch erwähnenswert, dass Clojure eine spezielle Syntax verwendet, um Zeichenfolgen zu verketten, wenn die Argumente nicht gleichzeitig ausgewertet werden sollen. Dazu müssen wir den `str` Aufruf in runde Klammern einschließen:

```Clojure
(str "Die Summe von" (+ 2 2) "und" (* 2 3) "ist") ;; Ausgabe: Die Summe von 4 und * ist

(str "Die Summe von" (+ 2 2) "und" (* 2 3) "ist") ;; Ausgabe: Die Summe von 4 und * ist
```

Dies sorgt dafür, dass die Auswertung der Argumente verzögert wird, bis die `str` Funktion aufgerufen wird. Dies kann nützlich sein, wenn wir komplexe Berechnungen in unsere Zeichenfolgen einbinden wollen.

## Siehe auch

- [Clojure Dokumentation zu `str`](https://clojuredocs.org/clojure.core/str)
- [Verschiedene Möglichkeiten, Zeichenfolgen in Clojure zu verketten](https://www.aitowert.de/blog/2015/06/01/3-ways-to-concatenate-strings-in-clojure/)