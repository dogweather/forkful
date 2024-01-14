---
title:                "Clojure: :Søking og utskifting av tekst"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Å søke og erstatte tekst er en viktig del av programmering, enten du trenger å korrigere feil eller gjøre større endringer i koden din. Det kan også være nyttig når du skal oversette en kodebase til et annet språk. Uansett årsak, så vil du lære hvordan man gjør det i Clojure!

## Slik gjør du det

For å søke og erstatte tekst i Clojure, kan du bruke funksjonen `replace`. Denne funksjonen tar inn tre argumenter: en streng (string) eller regex som skal søker etter, en streng som skal erstatte den søkte teksten, og til slutt en sekvens hvor du ønsker å utføre søket på. La oss si at vi ønsker å erstatte alle forekomster av "hallo" med "hei" i en liste:

```Clojure
(def liste ["hallo" "god morgen" "hallo" "hei" "hallo"])
(replace #"hallo" "hei" liste)
```

Dette vil resultere i følgende output:

```Clojure
["hei" "god morgen" "hei" "hei" "hei"]
```

Som vist i eksempelet, kan du bruke et regulært uttrykk (regex) i søket ditt ved å bruke `#""` rundt teksten du søker etter. Dette kan være nyttig hvis du ønsker å gjøre mer komplekse søk.

## Dypdykk

Det er verdt å merke seg at funksjonen `replace` ikke endrer på den opprinnelige sekvensen, men returnerer en ny sekvens med endringene. Hvis du ønsker å endre den opprinnelige sekvensen, kan du bruke funksjonen `replace!` i stedet.

Det finnes også en annen funksjon kalt `replace-first` som, som navnet tilsier, kun erstatter den første forekomsten av teksten du søker etter. Den fungerer på samme måte som `replace` ellers.

## Se også

- [`replace` dokumentasjon](https://clojuredocs.org/clojure.core/replace)
- [Regulære uttrykk i Clojure](https://clojure.org/api/cheatsheet#regex)
- [Andre Clojure funksjoner for tekstmanipulering](https://clojuredocs.org/clojure.string)