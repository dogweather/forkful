---
title:                "Clojure: Skriver til standardfeil"
simple_title:         "Skriver til standardfeil"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standardfeil (standard error) i Clojure kode kan være en nyttig måte å feilsøke og kommunisere med utvikleren på. Det lar deg informere om feilmeldinger, advarsler og andre viktige meldinger som er viktige for å forstå og løse problemer i koden din.

## Slik gjør du det

For å skrive til standardfeil i Clojure, bruker du funksjonen `println` og angir `System/err` som første parameter. Dette forteller Clojure at du vil skrive til standardfeil i stedet for standard utgang (standard out). Her er et eksempel på hvordan du kan bruke denne funksjonen:

```Clojure
(println System/err "Dette er en feilmelding som vil bli skrevet til standardfeil")
```

Output vil se slik ut:

```Clojure
Dette er en feilmelding som vil bli skrevet til standardfeil
```

I tillegg til `println` funksjonen kan du også bruke `eprintln` funksjonen for å skrive til standardfeil. Forskjellen er at `eprintln` automatisk legger til en linjeskift på slutten, så du trenger ikke å legge til det selv som i eksempelet over.

## Dypdykk

Når du skriver til standardfeil i Clojure, er det viktig å merke seg at meldingene dine vil bli skrevet til terminalen der koden din blir kjørt. Dette betyr at du kan se feilmeldinger og andre meldinger i sanntid når du kjører koden din.

En annen viktig ting å huske på er at du bør unngå å bruke standardfeil til å legge til logging i koden din. Dette kan føre til at feilmeldinger og andre viktige meldinger blir oversett og blandet sammen med loggingen din.

## Se også

- [Clojure dokumentasjonen om standardfeil](https://clojuredocs.org/clojure.core/println)
- [En grundig guide til Clojure feilsøking](https://technomancy.us/149)
- [En introduksjon til Clojure logging](https://clojure.org/guides/logging)