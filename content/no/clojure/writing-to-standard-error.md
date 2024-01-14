---
title:                "Clojure: Skriving til standardfeil"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standard error kan være en nyttig måte å håndtere feil og debugging på når du utvikler Clojure-programmer. Det lar deg sende informative meldinger direkte til din programvares feilstrøm, noe som kan hjelpe deg med å identifisere og løse problemer raskere.

## Hvordan gjøre det

Det første trinnet for å kunne skrive til standard error i Clojure er å importere `clojure.java.io` biblioteket. Dette gir deg tilgang til funksjoner som lar deg skrive til forskjellige strømmer, inkludert standard error. Her er et eksempel på hvordan du kan skrive en feilmelding til standard error:

```Clojure
(import '[clojure.java.io :as io])
(io/println *err* "Dette er en feilmelding!")
```

Dette vil skrive ut teksten "Dette er en feilmelding!" til feilstrømmen. Merk at `*err*` er et Clojure globalt varibelt som refererer til standard error-strømmen. Du kan bruke `*err*` når du vil sende meldinger til standard error.

## Dypdykk

Å skrive til standard error kan være spesielt nyttig når du jobber med flertrådede applikasjoner. Det lar deg skrive meldinger til feilstrømmen uten å påvirke utstrømmen til brukeren. Dette er nyttig når du vil holde kommunikasjon med brukeren ren og ryddig, men fortsatt være i stand til å få tilgang til feilmeldinger.

Du kan også bruke funksjoner som `with-out-str` for midlertidig å omdirigere utstrømmen til en annen strøm, inkludert standard error. Dette kan være nyttig når du jobber med biblioteker som allerede skriver ut til utstrømmen, men du vil kunne lese og håndtere feilmeldinger på en annen strøm.

## Se også

- [clojure.java.io dokumenasjon](https://clojuredocs.org/clojure.java.io)
- [Java IO API dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/io/package-summary.html)
- [Offisiell Clojure-nettside](https://clojure.org/)