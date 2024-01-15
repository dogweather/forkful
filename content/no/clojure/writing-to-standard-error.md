---
title:                "Skriving til standardfeil"
html_title:           "Clojure: Skriving til standardfeil"
simple_title:         "Skriving til standardfeil"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Skjermede programmeringsspråk har en tendens til å skjule feil og krasjer fra brukeren. Dette gjør debugging vanskeligere og kan føre til frustrasjon hos utviklere. Ved å skrive til standard error, kan feilmeldinger og krasjer vises tydelig, noe som gjør det enklere å identifisere og løse problemer under utviklingsprosessen.

## Hvordan du gjør det

```Clojure
(System/setErr (java.io.PrintWriter. (System/err))) ;Setter standard error til å bruke PrintWriter

(System/err :println "Feilmelding her") ;Skriver til standard error

```
Output:
```
Feilmelding her
```

## Dypdykk

Når du skriver til standard error, er det viktig å merke seg at feilmeldinger og krasjer som vises, også vil vises for brukeren av programmet. Derfor bør du begrense bruken av dette til utviklingsstadiet, og heller bruke standard output når programmet er klart for brukere. Du kan også styre hvordan feilmeldinger og krasjer vises ved å bruke forskjellige formateringsfunksjoner som `format` og `println` i kombinasjon med `System/err`.

## Se også

- [Logging i Clojure](https://clojure.org/guides/logging)
- [System Vars i Clojure](https://clojuredocs.org/clojure.core/binding)