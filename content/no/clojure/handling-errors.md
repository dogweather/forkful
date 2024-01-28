---
title:                "Feilhåndtering"
date:                  2024-01-26T00:51:08.699993-07:00
model:                 gpt-4-1106-preview
simple_title:         "Feilhåndtering"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/handling-errors.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Feilhåndtering dreier seg om å håndtere det uventede i programmer—ligner på en dørvakt som takler bråkmakere. Programmerere liker det problemfritt; feilhåndtering hjelper med å holde trøbbel i sjakk og sikrer at koden deres ikke snubler og faller når den står overfor det uventede.

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
