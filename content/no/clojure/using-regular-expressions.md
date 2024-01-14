---
title:                "Clojure: Å bruke regulære uttrykk"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Å bruke regulære uttrykk er en kraftig måte å søke og manipulere tekst på i Clojure. Det kan hjelpe deg med å finne spesifikke mønstre i en tekst, erstatte deler av en streng eller validere data. Regulære uttrykk kan også brukes i ulike programmeringsspråk og tekstbehandlingsprogrammer, så det er en nyttig ferdighet å ha.

## Hvordan

For å bruke regulære uttrykk i Clojure bruker vi funksjonene `re-seq` og `re-find`. `re-seq` returnerer en sekvens av treff for et gitt uttrykk, mens `re-find` returnerer det første treffet som en streng. La oss se på et eksempel der vi bruker regulære uttrykk til å finne alle ord som begynner med en stor bokstav i en tekst.

```Clojure
(def tekst "Dette er en tekst med noen store bokstaver.")

(re-seq #"[A-Z][a-z]*" tekst)
;; => ("Dette" "tekst" "store" "bokstaver")

(re-find #"[A-Z][a-z]*" tekst)
;; => "Dette"
```

I eksempelet bruker vi `re-seq` for å finne alle ord som starter med en stor bokstav og `re-find` for å finne det første ordet som gjør det. Vi bruker også regulære uttrykk for å spesifisere mønsteret vi søker etter, i dette tilfellet et ord som starter med en stor bokstav og har en eller flere små bokstaver etter det.

## Dypdykk

Regulære uttrykk kan være komplekse og vanskelige å lære, men de kan være utrolig nyttige når du blir komfortabel med dem. Hvis du ønsker å lære mer, anbefaler vi å se på følgende ressurser:

- **Mastering Regular Expressions**: en omfattende bok om regulære uttrykk som dekker alt fra de grunnleggende til avanserte teknikker.
- **RegexOne**: en interaktiv nettside som lærer deg regulære uttrykk ved å løse forskjellige oppgaver.
- **ClojureDocs**: en samling av Clojure-dokumentasjon som inkluderer informasjon om regulære uttrykk og relaterte funksjoner.

## Se også

- [Mastering Regular Expressions](https://www.amazon.com/Mastering-Regular-Expressions-Jeffrey-Friedl/dp/0596528124)
- [RegexOne](https://regexone.com/)
- [ClojureDocs](https://clojuredocs.org/)