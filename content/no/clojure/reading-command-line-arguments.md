---
title:                "Lese kommandolinjeargumenter"
date:                  2024-01-20T17:55:37.060608-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lese kommandolinjeargumenter"

category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å lese kommandolinjeargumenter betyr å hente input direkte fra terminalen når du starter et program. Programmerere gjør dette for å tillate brukerinnstillinger og dynamisk oppførsel uten å endre koden.

## Hvordan:
Bruk `*command-line-args*` i Clojure for å snappe opp argumenter. Her er et enkelt eksempel:

```Clojure
;; Hovedfunksjonen som printer alle kommandolinjeargumenter
(defn -main [& args]
  (doseq [arg args]
    (println arg)))
```

Når du kjører programmet:
```
$ clojure mitt_program.clj Hei Verden
Hei
Verden
```

Du kan også prosessere dem for spesifikk logikk:
```Clojure
(defn -main [& args]
  (let [greeting (first args)
        name (second args)]
    (println (str greeting ", " name "!"))))
```

Med dette programmet:
```
$ clojure mitt_program.clj Hei Clojure
Hei, Clojure!
```

## Deep Dive
Clojure, et moderne Lisp-dialekt, behandler command-line-argumenter som en liste. Dette kom fra dens Lisp-arv, hvor listehåndtering er kjernefunksjonaliteten.

Alternativer? Biblioteker som `tools.cli` tilbyr robust parsing for mer komplekse behov. Disse kan håndtere flagg, valg og validere argumenter.

Implementasjonsdetaljer? Clojure ligger på JVM (Java Virtual Machine) og setter `*command-line-args*` til en sekvens basert på Java sitt `String[] args`.

## Se Også
- Clojure's `tools.cli`: [https://github.com/clojure/tools.cli](https://github.com/clojure/tools.cli)
- Offisiell Clojure Dokumentasjon om script: [https://clojure.org/guides/deps_and_cli](https://clojure.org/guides/deps_and_cli)
- Inngående guide til Clojure: [https://clojure.org/guides/getting_started](https://clojure.org/guides/getting_started)
