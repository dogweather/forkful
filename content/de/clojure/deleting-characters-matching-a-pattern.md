---
title:    "Clojure: Löschen von Zeichen, die einem Muster entsprechen"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann in verschiedenen Szenarien sehr nützlich sein. Zum Beispiel kannst du damit unerwünschte Zeichen aus einer Zeichenkette entfernen oder eine bessere Datenverarbeitung in deinem Programm ermöglichen. In diesem Blogbeitrag werden wir uns ansehen, wie du dies in Clojure umsetzen kannst.

## So geht's

Um Zeichen basierend auf einem Muster zu löschen, verwenden wir die `clojure.string/replace` Funktion und geben das Muster und einen leeren String als Argumente an. Hier ist ein Beispielcode, der alle Vokale aus einem Satz löscht:

```Clojure
(let [satz "Das ist ein Beispieltext."]
   (clojure.string/replace satz #"[aeiou]" ""))
```
Das Ergebnis dieser Operation ist "Ds st n Bspltxt." Die `replace` Funktion durchläuft den Satz und überschreibt alle Vokale mit einem leeren String, wodurch sie gelöscht werden.

Du kannst auch reguläre Ausdrücke verwenden, um komplexe Muster zu erkennen und zu löschen. In diesem Beispiel ersetzen wir alle Zahlen in einem String mit einem leeren String:

```Clojure
(let [text "Dies ist eine Nummer123."]
   (clojure.string/replace text #"\d" ""))
```
Das Ergebnis ist "Dies ist eine Nummer." Hier wird der reguläre Ausdruck `"\d"` verwendet, der alle Zahlen im String erkennt und durch ein leeres String ersetzt.

## Tiefergehender Einblick

Clojure bietet auch die `clojure.string/replace-first` Funktion, die nur die erste Übereinstimmung im String ersetzt. Dies kann in bestimmten Fällen nützlicher sein als `replace`, das alle Übereinstimmungen ersetzt. Hier ist ein Beispielcode, der nur das erste Vorkommen einer Zeichenkette ersetzt:

```Clojure
(let [text "Hello hello there"]
   (clojure.string/replace-first text "hello" ""))
```
Das Ergebnis ist " Hello there". Die `replace-first` Funktion ersetzt nur das erste "hello" im Satz, während `replace` alle ersetzen würde.

Eine weitere nützliche Funktion ist `clojure.string/replace-iter`, die eine Funktion auf alle Übereinstimmungen im String anwendet. Hier ist ein Beispiel, das alle Zahlen im String verdoppelt:

```Clojure
(let [text "Die ersten 2 Zahlen sind 1 und 2"]
   (clojure.string/replace-iter text #"\d" #(format "%d%d" (Integer/parseInt %) (Integer/parseInt %))))
```
Das Ergebnis ist "Die ersten 4 Zahlen sind 2 und 4". Die `replace-iter` Funktion wendet die angegebene Funktion auf alle Zahlen im String an und verdoppelt sie.

## Siehe auch

- [Clojure String Dokumentation](https://clojuredocs.org/clojure.string)
- [Reguläre Ausdrücke in Clojure](https://clojuredocs.org/clojure.string#clojure.string/replace)
- [Praktische Einführung in Clojure](https://www.clojure-for-beginners.com/)