---
title:                "Clojure: Stor bokstaver på en streng"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Det kan være mange grunner til å ville formatere en tekst på en bestemt måte, for eksempel å øke lesbarheten eller for å følge gitte konvensjoner. I Clojure, kan det være nyttig å kunne konvertere en streng til store bokstaver for å sørge for konsistens i koden din.

## Hvordan få det til

Det finnes flere måter å kapitalisere en streng på i Clojure, avhengig av hvordan du ønsker å bruke resultatet. Her er to eksempler:

```
Clojure (capitalize "hello world")
```
Dette vil gi outputen "Hello world"

```
Clojure (clojure.string/upper-case "hello world")
```
Dette vil gi outputen "HELLO WORLD"

## Dypdykk

Hvis du vil ha mer kontroll over hvordan strengen blir formatert, kan du bruke Clojures string-funksjoner. For eksempel kan du bruke "replace-first" for å kapitalisere bare den første bokstaven i en streng, eller "replace" for å kapitalisere alle bokstaver i strengen.

En annen nyttig funksjon er "title-case", som vil kapitalisere hvert ord i en streng.

```
Clojure (clojure.string/replace-first "hello world" #"h" "H")
```
Dette vil gi outputen "Hello world"

```
Clojure (clojure.string/title-case "hello world")
```
Dette vil gi outputen "Hello World"

Det finnes også biblioteker som kan hjelpe med kapitalisering av strenger, som for eksempel "stringcase" og "clj-capitalize".

## Se også

- [Clojure string functions](https://clojuredocs.org/clojure.string)
- [Stringcase library](https://github.com/jarohen/stringcase)
- [Clj-capitalize library](https://github.com/athos/clj-capitalize)