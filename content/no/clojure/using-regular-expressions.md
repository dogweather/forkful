---
title:                "Å bruke regulære uttrykk"
html_title:           "Arduino: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Bruken av regulære uttrykk (Regular Expressions eller Regex) er et kraftig verktøy for strengmanipulering innen programmering. Programmerere bruker det hovedsakelig for matching, selektering og splitting av tekststrenger basert på definerte mønstre.

## Hvordan bruke:

Vi vil bruke Clojure's `re-seq` funksjon for å illustrere bruken av Regex.

```Clojure 
(def teksten "Hei På Deg, Norge123!")
(def mønsteret "\\w+")

(prn (re-seq (re-pattern mønsteret) teksten))

```

Output:
```
("Hei" "På" "Deg" "Norge123")
```
Mønsteret "\\w+" gir oss en sekvens av alle ordene i tekststrengen. 

## Dypere Inn:

Historisk sett, regulære uttrykk ble først brukt i Unix operativsystemer. Selv om bruk av regulære uttrykk kan være forvirrende for en nybegynenr, kan det være utrolig kraftig når det brukes riktig. Det er alternativer til regex, som string manipulasjon funksjoner, men disse tilbyr ikke samme nivå av fleksibilitet og kontroll. I termen av implementering, har Clojure en rekke regex funksjoner som `re-find`, `re-matches`, `re-seq`, og andre.

## Se Også: 

For ytterligere lesning og ressurser, se de følgende kildene:

- [Clojure Regex Manual](https://clojuredocs.org/clojure.core/re-seq)
- [Regular Expressions Intro](https://www.regular-expressions.info/tutorial.html)