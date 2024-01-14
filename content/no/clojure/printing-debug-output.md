---
title:                "Clojure: Utskrift av feilrettingsutdata"
programming_language: "Clojure"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive ut feilsøkingsmeldinger kan være en viktig del av utviklingsprosessen. Ved å skrive utmeldinger kan du enklere finne og løse feil i koden din. I denne bloggposten vil vi utforske hvordan man kan skrive ut debug meldinger i Clojure.

## Hvordan

Først må du importere "clojure.pprint" biblioteket for å kunne bruke funksjonen "pprint". Deretter kan du enkelt legge til debug meldinger ved hjelp av "pprint" funksjonen, som vil skrive ut objekter i en mer leselig format.

```Clojure
(ns min-prosjekt.core
  (:require [clojure.pprint :refer [pprint]]))

(defn funksjon [param1 param2]
  (pprint (str "Param1: " param1))
  (pprint (str "Param2: " param2))
  (pprint (str "Resultat: " (+ param1 param2))))
```

Når du kjører denne koden, vil du få følgende output:

```Clojure
Param1: Verdi1
Param2: Verdi2
Resultat: 3
```

Dette gjør det enklere å se hva som blir sendt inn i funksjonen og hva resultatet blir.

## Dypdykk

Hvis du ønsker å dykke dypere og skrive ut mer komplekse objekter, kan du bruke funksjonen "pprint-table". Denne funksjonen gjør at du kan skrive ut datastrukturer som kart, sett og vektorer på en mer organisert måte.

```Clojure
(ns min-prosjekt.core
  (:require [clojure.pprint :refer [pprint-table]]))

(def data {:navn "Maria" :alder 26 :yrke "Programmerer"})

(pprint-table data)
```

Output:

```Clojure
|----------------|
| :navn | :alder |
|-------+--------|
| Maria |    26  |
|----------------|
| :yrke |         |
|--------+--------|
|Programmerer|     |
|---------+-------|
```

Dette gjør det enklere å se strukturen til komplekse objekter, og kan være nyttig når du må analysere større datamengder.

## Se også

* [Clojure offisiell dokumentasjon] (https://clojure.org/)
* [Clojure feilsøking] (https://tech.lendingclub.com/clojure-debugging-101/)