---
title:                "Lese en tekstfil"
aliases:
- no/clojure/reading-a-text-file.md
date:                  2024-01-20T17:53:54.728538-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lese en tekstfil"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Å lese en tekstfil betyr å hente data fra en fil lagret på disken for å bruke den i et program. Programmerere gjør dette fordi filer ofte inneholder nyttig data som konfigurasjon, brukerdata eller annen tekst som skal behandles.

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
