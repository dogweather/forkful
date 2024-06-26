---
date: 2024-01-20 17:53:54.728538-07:00
description: "How to: Lesing av filer har v\xE6rt en grunnleggende del av programmering\
  \ siden tidlige datamaskiner. I Clojure, som er en JVM-spr\xE5k, benyttes Java sin\
  \ I/O\u2026"
lastmod: '2024-04-05T22:50:54.429496-06:00'
model: gpt-4-1106-preview
summary: "Lesing av filer har v\xE6rt en grunnleggende del av programmering siden\
  \ tidlige datamaskiner."
title: Lese en tekstfil
weight: 22
---

## How to:
Les en enkel tekstfil:
```Clojure
(with-open [rdr (java.io.BufferedReader. (java.io.FileReader. "path/til/din/fil.txt"))]
  (doseq [line (line-seq rdr)]
    (println line)))
```

Eksempelutdata:
```
Dette er linje en.
Dette er linje to.
Dette er linje tre.
```

Lag en funksjon for å lese alle linjene i en fil til en liste:
```Clojure
(defn les-fil-til-liste [filsti]
  (with-open [rdr (java.io.BufferedReader. (java.io.FileReader. filsti))]
    (doall (line-seq rdr))))
```

Bruk:
```Clojure
(les-fil-til-liste "path/til/din/fil.txt")
; => ("Dette er linje en." "Dette er linje to." "Dette er linje tre.")
```

## Deep Dive
Lesing av filer har vært en grunnleggende del av programmering siden tidlige datamaskiner. I Clojure, som er en JVM-språk, benyttes Java sin I/O API for å lese filer. Det er viktig å lukke filressurser etter bruk, noe `with-open` tar seg av. Andre metoder inkluderer `slurp` for små filer og NIO for store eller binære filer. For storskala prosessering kan du bruke biblioteker som Apache Tika for avansert innholdsanalyse eller overveie parallell lesing for ytelse.

## See Also
- ClojureDocs for `slurp` og `line-seq`: https://clojuredocs.org/
- Java I/O forståelse: https://docs.oracle.com/javase/tutorial/essential/io/
- Apache Tika for kompleks filbehandling: https://tika.apache.org/
