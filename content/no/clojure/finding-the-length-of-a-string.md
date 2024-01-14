---
title:                "Clojure: På jakt etter lengden til en tekststreng"
simple_title:         "På jakt etter lengden til en tekststreng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Strings, eller strenger, er en viktig del av programmering og kan ofte være nøkkelen til å manipulere og håndtere data. Å vite lengden på en streng er en grunnleggende operasjon som kan være nyttig i en rekke situasjoner. I denne bloggposten vil vi gå gjennom hvordan man kan finne lengden på en streng i Clojure.

## Hvordan
Å finne lengden på en streng i Clojure er enkelt og kan gjøres på flere måter.

```Clojure
(.length "Hei, verden!")
```
Dette eksempelet bruker Java-interoperabilitet i Clojure for å kalle på lengdefunksjonen til Java String-klassen.

```Clojure
(count "Hei, verden!")
```
Her bruker vi funksjonen "count" som er en del av Clojure's standardbibliotek. Denne funksjonen teller antall elementer i en sekvens, inkludert den gitte strengen.

Om du ønsker å finne lengden på alle strenger i en liste, kan du bruke "map" funksjonen.

```Clojure
(map count ["Hei, verden!" "Hello, world!"])
```

Dette vil returnere en sekvens med lengden på hver streng, som i dette tilfellet blir [13 12]. 

Alle eksemplene ovenfor vil gi oss samme utgangspunkt, nemlig lengden på den gitte strengen. Dette er en enkel og nyttig funksjon som kan brukes i mange forskjellige sammenhenger.

## Dypdykk
Det er viktig å merke seg at lengden på en streng er antall tegn i strengen, og ikke antall ord. Dette kan føre til forvirring hvis man ikke er klar over forskjellen.

En annen viktig ting å huske på er at "count" funksjonen kun opererer på Unicode-tegn og ignorerer eventuelle kontrollkarakterer eller hviteskipskarakterer. For å få en mer nøyaktig telling kan man bruke en regex for å fjerne disse karakterene før man teller lengden.

## Se også
- [Clojure dokumentasjon](https://clojuredocs.org/)
- [Java String dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Clojure regex tutorial](https://dzone.com/articles/java-regex-tutorial)