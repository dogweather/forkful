---
title:    "Clojure: Verkettung von Zeichenfolgen"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

In diesem Beitrag werden wir uns mit einem grundlegenden Konzept der Clojure-Programmierung beschäftigen: der Verkettung von Zeichenketten. Eine grundlegende, aber äußerst nützliche Technik, die Ihnen helfen wird, effizienter und eleganteren Code zu schreiben.

## Wie geht das?

Das Verkettung von Zeichenketten in Clojure ist sehr einfach und intuitiv. Hier ist ein Beispiel, wie Sie zwei Zeichenketten miteinander verketten können:

```Clojure
(str "Hallo, " "Welt!")
```

Die Ausgabe davon wäre:

```
"Hallo, Welt!"
```

Sie können auch Variablen in die Verkettung einbeziehen, indem Sie sie in die Klammern einschließen. Hier ein Beispiel:

```Clojure
(def name "Max")
(str "Hallo, " name "!")
```

Die Ausgabe wäre:

```
"Hallo, Max!"
```

Sie können auch mehrere Funktionen miteinander verketten, um komplexere Zeichenketten zu erstellen. Hier ein etwas komplizierteres Beispiel:

```Clojure
(def first-name "Max")
(def last-name "Mustermann")
(str "Mein vollständiger Name ist " (.toUpperCase first-name) " " (.toUpperCase last-name))
```

Die Ausgabe wäre:

```
"Mein vollständiger Name ist MAX MUSTERMANN"
```

## Tiefergehende Informationen

In Clojure werden Zeichenketten als Sequenzen von Zeichen dargestellt. Dies erlaubt es uns, Funktionen wie `map` oder `filter` auf Zeichenketten anzuwenden. Hier ein Beispiel mit `map`:

```Clojure
(def city "Berlin")
(map char city)
```

Die Ausgabe wäre:

```
(\B \e \r \l \i \n)
```

Sie können auch mehrere Zeichenketten miteinander verketten, indem Sie die Funktion `clojure.string/join` verwenden. Hier ein Beispiel:

```Clojure
(def fruits ["Apfel" "Banane" "Orange"])
(clojure.string/join ", " fruits)
```

Die Ausgabe wäre:

```
"Apfel, Banane, Orange"
```

## Siehe auch

- Tutorial zur Verwendung der `clojure.string` Bibliothek: https://clojuredocs.org/clojure.string
- Offizielle Clojure Dokumentation zu Zeichenkettenfunktionen: https://clojure.org/reference/strings