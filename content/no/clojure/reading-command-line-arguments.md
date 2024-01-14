---
title:    "Clojure: Lese kommandolinjeargumenter"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Hvorfor

Å lese kommandolinjetilordninger er en viktig ferdighet for enhver programmerer, spesielt når du arbeider med Clojure. Det gir deg muligheten til å ta imot og behandle input fra brukere, som kan være nyttig for å lage interaktive programmer eller automatisere oppgaver.

# Hvordan

For å lese kommandolinjetilordninger i Clojure, kan du bruke funksjonen *command-line-args*. Denne funksjonen vil returnere en liste med argumentene som ble gitt ved å starte programmene fra kommandolinjen. La oss se på et eksempel:

```Clojure
(defn print-args []
  (let [args (command-line-args)]
    (println "Antall argumenter:" (count args))
    (println "Argumenter:" args)))

(print-args)
```

Når du kjører dette programmet fra kommandolinjen med følgende kommando:

```bash
lein exec print-args.clj arg1 arg2 arg3
```

vil output være:

```bash
Antall argumenter: 3
Argumenter: (arg1 arg2 arg3)
```

Som du kan se, vil *command-line-args* funksjonen returnere en liste med alle argumentene som er gitt som input. Du kan deretter behandle disse argumentene videre for å oppnå ditt ønskede resultat.

# Dypdykk

Hvis du ønsker enda mer kontroll over hvordan kommandolinjetilordninger blir lest og behandlet, kan du bruke funksjonen *apply-args*. Denne funksjonen lar deg sende inn en funksjon som tar imot argumentene fra kommandolinjen og behandler dem på en bestemt måte. La oss se på et eksempel:

```Clojure
(defn multiply-args [args]
  (apply * args))

(let [args (command-line-args)]
  (println "Resultat:" (multiply-args args)))
```

Kjører dette programmet med følgende kommando:

```bash
lein exec multiply-args.clj 5 3
```

vil output være:

```bash
Resultat: 15
```

Som du kan se, kan du med *apply-args* gi en personalisert funksjon for å behandle kommandolinjetilordninger.

# Se også

- [Clojure kommandolinjetilordninger dokumentasjon](https://clojuredocs.org/clojure.core/command-line-args)
- [En introduksjon til Clojure](https://clojure.org/guides/getting_started)
- [Mer om Clojure-syntaks](https://learnxinyminutes.com/docs/clojure/)