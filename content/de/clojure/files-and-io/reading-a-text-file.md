---
date: 2024-01-20 17:54:00.923897-07:00
description: "How to: Das Auslesen von Textdateien ist grundlegend und wurde schon\
  \ in fr\xFChen Programmiersprachen wie C implementiert. Clojure, eine moderne Lisp-\u2026"
lastmod: '2024-04-05T21:53:55.394002-06:00'
model: gpt-4-1106-preview
summary: "Das Auslesen von Textdateien ist grundlegend und wurde schon in fr\xFChen\
  \ Programmiersprachen wie C implementiert."
title: Textdatei einlesen
weight: 22
---

## How to:
```Clojure
;; Datei lesen
(slurp "pfad/zur/datei.txt")
```
Output:
```
"Das ist der Inhalt deiner Datei"
```

```Clojure
;; Zeilenweise lesen
(with-open [r (reader "pfad/zur/datei.txt")]
  (doall (line-seq r)))
```
Output:
```
("Erste Zeile" "Zweite Zeile" "Dritte Zeile")
```

```Clojure
;; Datei mit Ausnahmebehandlung lesen
(try
  (slurp "pfad/zur/datei.txt")
  (catch Exception e
    (str "Fehler beim Lesen der Datei: " (.getMessage e))))
```
Output bei Fehler:
```
"Fehler beim Lesen der Datei: Datei nicht gefunden"
```

## Deep Dive:
Das Auslesen von Textdateien ist grundlegend und wurde schon in frühen Programmiersprachen wie C implementiert. Clojure, eine moderne Lisp-Variante, macht es simpel. Die Funktion `slurp` ist bequem, jedoch nicht effizient für große Dateien. `Line-seq` und der `with-open`-Block verhindern Speicherprobleme durch Lazy Loading der Zeilen. Alternativen wie `java.io.BufferedReader` bieten mehr Kontrolle, sind aber komplexer.

## See Also:
- [ClojureDocs zu `slurp`](https://clojuredocs.org/clojure.core/slurp)
- [ClojureDocs zu `line-seq`](https://clojuredocs.org/clojure.core/line-seq)
