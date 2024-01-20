---
title:                "Gjør en streng stor"
html_title:           "Clojure: Gjør en streng stor"
simple_title:         "Gjør en streng stor"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Store bokstaver i en tekststreng handler om å endre alle begynnelsesbokstaver i ordene til store bokstaver. Programmerer gjør dette for lesbarhet og data normalisering.

## Hvordan til:
Her er hvordan vi gjør det i Clojure, med en kodeeksempel:

```clojure
(require '[clojure.string :as str])

(defn capitalize
  [text]
  (->> (str/split text #" ")
       (map str/capitalize)
       (str/join " ")))
```

La oss teste det ut:

```clojure
(println (capitalize "Hallo, verden!")) 
```

Dette vil skrive ut:

```clojure
"Hallo, Verden!"
```

## Dypdykk
Store bokstaver i strenger har en lang historie, med røtter fra eldre programmeringsspråk som Cobol og Fortran. 

Clojure tilbyr mange alternative måter å takle denne oppgaven, takket være sin fleksible syntaks og fokus på funksjonell programmering. Du kan også bruke regulære uttrykk for å matche og erstatte små bokstaver, eller utnytte Java Bibliotek funksjoner som Java's 'java.lang.Character/toTitleCase'.

Fordi Clojure kjører på JVM (Java Virtual Machine), bruker vår funksjon underliggende Java-metode `java.lang.Character/toTitleCase` for å utføre selve kapitaliseringen.

## Se også:
For et dypere dykk inn i tekstmanipulasjon i Clojure, sjekk ut disse kildene:


2. [Clojure Docs](https://clojuredocs.org): En omfattende kilde til dokumentasjon som dekker Clojure's mange innebygde funksjoner, inkludert de for tekstmanipulasjon.

3. [Official Clojure API Reference](https://clojure.github.io/clojure/): Den offisielle API-referansen for Clojure, inkludert detaljerte beskrivelser av funksjonene i string-biblioteket.