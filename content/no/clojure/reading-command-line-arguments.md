---
title:                "Lese kommandolinjeargumenter"
html_title:           "Arduino: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?

Å lese kommandolinjeargumenter er et hjelpsomt triks for å tillate brukerinteraksjon utenfor selve programmet. Programmerere gjør dette for å tillate dynamisk innhold endringer basert på brukerens valg.

## Hvordan:

Her presenteres hvordan du kan lese kommandolinjeargumenter i Clojure:

```Clojure
(defn -main
   "Dette leser argumenter fra kommandolinja"
   [& args]
   (println "Du ga disse argumentene:" args))

```
Hvis du kjører dette programmet med: 
```
lein run hei hallo
```
Det vil skrive ut:
```
Du ga disse argumentene: [hei hallo]
```

## Dypdykk

Å kunne lese argumenter fra kommandolinjen er en eldgammel teknikk, og eksisterte til og med i gamle dager av å skrive direkte på maskinkoden. Det finnes alternative måter å håndtere dette på, for eksempel å lese fra en konfigurasjonsfil, bruke miljøvariabler, eller til og med gjøre nettverkskall. Imidlertid er hver av disse metodene mer komplekse og krever mer kode. Clojure håndterer dette på en veldig funksjonell måte, og tillater en liste med argumenter å bli behandlet akkurat som en hvilken som helst annen funksjonell sekvens.

## Se Også

- [Kommandolinjeargumenter i Clojure - Stack Overflow](https://stackoverflow.com/questions/3960063/clojure-command-line-arguments)
- [Clojure - offisiell dokumentasjon](https://clojure.org/guides/getting_started)
- [Begynn å skrive Clojure - læringstur](https://learnxinyminutes.com/docs/clojure/)