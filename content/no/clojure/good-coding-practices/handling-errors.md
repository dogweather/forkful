---
date: 2024-01-26 00:51:08.699993-07:00
description: "Hvordan: Clojure, i likhet med sine Lisp forfedre, st\xF8tter seg p\xE5\
  \ unntak for \xE5 h\xE5ndtere feil. Slik viser du hva du er laget av n\xE5r ting\
  \ g\xE5r d\xE5rlig. \xC5\u2026"
lastmod: '2024-03-13T22:44:40.409430-06:00'
model: gpt-4-1106-preview
summary: "Clojure, i likhet med sine Lisp forfedre, st\xF8tter seg p\xE5 unntak for\
  \ \xE5 h\xE5ndtere feil."
title: "Feilh\xE5ndtering"
weight: 16
---

## Hvordan:
Clojure, i likhet med sine Lisp forfedre, støtter seg på unntak for å håndtere feil. Slik viser du hva du er laget av når ting går dårlig.

Å kaste et unntak er rett frem:
```Clojure
(throw (Exception. "Oops! Noe gikk galt."))
```

Å fange et unntak, dette vil du gjøre mye:
```Clojure
(try
  ;; risikabel kode
  (/ 1 0)
  (catch ArithmeticException e
    (println "Kan ikke dele på null!"))
  ;; finally-blokken kjøres uansett
  (finally 
    (println "Oppryddingskode kommer her.")))
```
Eksempel på output for catch-blokken over:
```
Kan ikke dele på null!
Oppryddingskode kommer her.
```

Bruke `ex-info` og `ex-data` for rikere kontekst om unntak:
```Clojure
(try
  ;; forårsaker en tilpasset unntak
  (throw (ex-info "egendefinert feil" {:type :egendefinert-feil}))
  (catch Exception e
    ;; å hente ut data fra vårt egen definerte unntak
    (println (ex-data e))))
```
Eksempel på output:
```
{:type :egendefinert-feil}
```

## Dypdykk
Historien om feilhåndtering i Clojure er ikke radikalt forskjellig fra andre Lisps eller til og med Java (som den arver `try-catch` mekanismen fra). Det er pragmatisk; å bruke unntak er hovedstien, akkurat som i Java, men Clojure tilbyr en funksjonell vri med `ex-info` og `ex-data` for rikere feildata.

Alternativer for feilhåndtering i Clojure inkluderer bruk av monadiske konstruksjoner, slik som `either` monaden fra biblioteker som `cats`, eller core.async for kanalbasert feilpropagering. Imidlertid er disse mer komplekse og brukes i spesifikke scenarier.

Historisk sett har feilhåndtering i programmeringsspråk utviklet seg fra enkle statusreturer til de mer sofistikerte unntakshåndteringsmekanismene i moderne språk. Clojure velger enkelhet og et snev av funksjonell programmering, og blander gammelt og nytt.

## Se også
- Clojures veiledning til unntak: https://clojure.org/guides/exceptions
- “Cats” biblioteket for mer funksjonelle tilnærminger: https://github.com/funcool/cats
- “Core.async” for asynkron programmering: https://github.com/clojure/core.async
