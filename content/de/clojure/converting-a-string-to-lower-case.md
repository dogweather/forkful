---
title:                "Clojure: Umwandeln eines Strings in Kleinbuchstaben"
simple_title:         "Umwandeln eines Strings in Kleinbuchstaben"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren eines Strings in Kleinbuchstaben ist eine häufig benötigte Funktion beim Programmieren. Es ermöglicht uns, Texte einheitlich zu formatieren und erleichtert somit die weitere Verarbeitung.

## Wie geht's

Um einen String in Kleinbuchstaben umzuwandeln, können wir die `lower-case` Funktion verwenden. Diese Funktion akzeptiert einen einzigen Parameter, den zu konvertierenden String.

```Clojure
(lower-case "Hallo WELT") ; Output: "hallo welt"
```

Ein weiterer Weg ist die Verkettung der `str` Funktion mit der `clojure.string/lower-case` Funktion. Dies ermöglicht eine bessere Lesbarkeit und  ist besonders hilfreich, wenn wir zusätzliche Manipulationen am String vornehmen möchten.

```Clojure
(str (clojure.string/lower-case "HALLo")) ; Output: "hallo"
```

## Tiefer Einblick

Beim Konvertieren in Kleinbuchstaben kann es zu unerwartetem Verhalten kommen, je nachdem, welche Zeichen im String enthalten sind. Zum Beispiel kann die Großbuchstaben-I mit Akut (Ì) nicht einfach in einen Kleinbuchstaben umgewandelt werden. Stattdessen bleibt es ein unverändertes Zeichen.

```Clojure
(lower-case "Ã‰è") ; Output: "Ã©Ã¨"
```

Um dieses Problem zu lösen, können wir die `Unicode`-Bibliothek verwenden. Sie bietet die `normalization` Funktion, die uns hilft, Sonderzeichen in ASCII-Zeichen umzuwandeln. Dann können wir die `lower-case` Funktion wie gewohnt verwenden.

```Clojure
(require '[clojure.string :refer [lower-case]]
         '[clojure.data :as data]
         '[clojure.java.io :as io]
         '[clojure.pprint :refer [print-table]])

(defn convert-text [text]
  (lower-case (apply str (data/normalization :nfd text))))

(defn read-text-from-file [file]
  (reduce str 
          (with-open [r (io/reader file)]
            (line-seq r))))

(def sample-text (read-text-from-file "sample.txt"))

(print-table [["Original Text" "Converted Text"]
              [sample-text (convert-text sample-text)]])
```

Und der Output sieht jetzt so aus:

```
| Original Text | Converted Text |
|---------------+----------------|
| liebe GrÃ¼ÃŸe  | liebe grüße   |
```

## Siehe auch

- [Clojure Dokumentation zu lower-case](https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/lower-case)
- [Clojure Unicode Bibliothek](https://clojuredocs.org/clojure.java.io/reader)