---
title:                "Finne lengden på en streng"
html_title:           "Arduino: Finne lengden på en streng"
simple_title:         "Finne lengden på en streng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å finne lengden på en streng betyr å telle antall tegn en streng inneholder. Det er en grunnleggende operasjon i programmering som brukes for å manipulere data, utføre valideringer, og løse mange daglige programmeringsoppgaver.

## Hvordan:

Clojure benytter funksjonen `count` for å beregne antall elementer i en samling, eller antall tegn i en streng. Her er et enkelt eksempel:

```Clojure
(def string-lenght-example 
     (fn [input-string] 
        (count input-string)))

(println "Strenglengden er : " (string-lenght-example "Hei, Norge"))
```
Når du kjører koden ovenfor, skal det returneres følgende: 
```Clojure
Strenglengden er : 10
```

## Dypdykk

1. Historisk kontekst: I eldre programmeringsspråk, var det utfordrende å finne lengden på en streng. For eksempel, i C måtte man iterere gjennom hvert tegn til man fant det avsluttende null-tegnet. Med høy-nivå språk som Clojure, er det betydelig enklere.

2. Alternativer: Selv om `count`-funksjonen er den vanligste måten å finne lengden til en streng i Clojure, kan andre funksjoner som `size` eller `length` bli brukt i forskjellige sammenhenger og samlinger.

3. Implementeringsdetaljer: I Clojure implementeres `count`-funksjonen ved å kalle Java's innebygde `length`-metode. Det driver rask og nøyaktig beregning av strenglengden.

## Se Også

For mer informasjon om strenger og deres behandling i Clojure, sjekk ut følgende ressurser:

- "Clojure for the Brave and True" av Daniel Higginbotham (http://www.braveclojure.com/core-functions-in-depth/)
- Offisiell Clojure dokumentasjon - Count (https://clojuredocs.org/clojure.core/count)
- Clojure - Strenger (https://clojure.org/guides/learn/strings)