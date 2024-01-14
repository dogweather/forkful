---
title:    "Clojure: Das Lesen einer Textdatei"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Warum 

Wahrscheinlich bist du hier, weil du lernen willst, wie man Textdateien in Clojure liest. Das kann aus verschiedenen Gründen nützlich sein, zum Beispiel um Daten zu analysieren oder sie in einem Programm zu verwenden. In diesem Blog-Post werde ich dir zeigen, wie es geht.

## Wie man Textdateien in Clojure liest

Um eine Textdatei in Clojure zu lesen, verwenden wir die Funktion `slurp`. Diese Funktion liest den gesamten Inhalt der Datei als String ein. Hier ist ein Beispiel, wie man eine Datei mit dem Namen "beispiel.txt" einliest:

```Clojure
(slurp "beispiel.txt")
```

Das Ergebnis wird ein String sein, der den gesamten Inhalt der Datei enthält. Wenn die Datei zum Beispiel folgenden Inhalt hat:

```
Hallo Welt!
Dies ist ein Beispieltext.
```

Das Ergebnis wäre der String "Hallo Welt! Dies ist ein Beispieltext.".

Wenn du jedoch den Inhalt der Datei in einer Liste haben möchtest, kannst du die Funktion `line-seq` verwenden. Diese Funktion liest die Datei Zeile für Zeile ein und gibt eine Liste von Strings zurück. Hier ist ein Beispiel:

```Clojure
(line-seq (io/reader "beispiel.txt"))
```

Das Ergebnis wäre eine Liste mit den Strings "Hallo Welt!" und "Dies ist ein Beispieltext.".

## Tieferer Einblick

Es ist wichtig zu beachten, dass sowohl `slurp` als auch `line-seq` den Dateipfad als String als Argument erwarten. Wenn du also eine Datei im gleichen Ordner wie dein Clojure-Programm hast, musst du nur den Dateinamen angeben. Wenn sich die Datei in einem anderen Ordner befindet, musst du den vollständigen Pfad angeben.

Es ist auch möglich, eine bestimmte Codierung zu spezifizieren, wenn die Datei nicht im Standardformat (UTF-8) ist. Dies kann mit der Funktion `with-open` erreicht werden, die einen Dateipfad und eine Codierung als Argumente erwartet. Hier ist ein Beispiel:

```Clojure
(with-open [rdr (io/reader "beispiel.txt" :encoding "ISO-8859-1")]
  (line-seq rdr))
```

Dieses Beispiel würde dasselbe Ergebnis zurückgeben wie das vorherige, jedoch mit der spezifizierten Codierung.

## Siehe auch

* [Official Clojure Documentation on slurp] (https://clojuredocs.org/clojure.core/slurp)
* [Official Clojure Documentation on line-seq] (https://clojuredocs.org/clojure.core/line-seq)