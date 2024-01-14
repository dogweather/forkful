---
title:                "Clojure: Å finne lengden til en streng"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
I denne bloggen skal vi se på hvordan du kan finne lengden til en tekststreng ved hjelp av Clojure programmeringsspråk. Å finne lengden av en streng er en grunnleggende oppgave som ofte brukes i utvikling av dataprogrammer, og det er derfor viktig å forstå hvordan man kan gjøre dette i Clojure.

## Hvordan
For å finne lengden til en streng i Clojure, kan vi bruke funksjonen "count". Denne funksjonen tar inn en streng som argument og returnerer antall tegn i strengen. La oss se på et eksempel:

```Clojure
(count "Hei, verden!")
```

Dette vil returnere 12, siden strengen "Hei, verden!" inneholder 12 tegn. Vi kan også bruke "count" på variabler som inneholder strenger.

```Clojure
(def tekst "Dette er en test")
(count tekst)
```

Dette vil også returnere 12. Det er viktig å merke seg at "count" også kan brukes på andre datatyper som lister og vektorer, og vil da returnere antall elementer i disse.

## Deep Dive
Når vi bruker "count" funksjonen på en streng, hva skjer egentlig bak kulissene? I Clojure er strenger implementert som en sekvens av tegn, og "count" funksjonen går gjennom denne sekvensen og teller antall tegn. Dette kan være nyttig å vite hvis du vil lage din egen funksjon for å finne lengden av en streng.

Det er også viktig å merke seg at "count" funksjonen tar hensyn til Unicode-tegn i strengen, og ikke bare standard ASCII-tegn. Dette betyr at den vil fungere for å finne lengden av både engelske og norske tekster.

## Se Også
- [Clojure dokumentasjon for "count" funksjonen](https://clojuredocs.org/clojure.core/count)
- [En tutorial om strenger i Clojure](https://www.braveclojure.com/do-things-with-strings/)
- [En guide til Unicode i Clojure](https://gist.github.com/skeet70/cef7dad4b55720326d1451ceff2ea3d7)