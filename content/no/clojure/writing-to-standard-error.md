---
title:                "Skriving til standardfeil"
html_title:           "Clojure: Skriving til standardfeil"
simple_title:         "Skriving til standardfeil"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Skriver du noen ganger til standard error i Clojure? Det er når vi ønsker å skrive feilmeldinger eller annen utdata til en annen strøm enn standard output. Dette kan være nyttig for å skille feilmeldinger fra vanlig utdata eller for å vise mer detaljert informasjon om feil.

## Slik gjør du det:
```Clojure
(System/err "Dette er en feilmelding")
```
Dette vil skrive ut "Dette er en feilmelding" til standard error strømmen. Du kan også bruke andre funksjoner, som prn eller prerrn, for å skrive ut data til standard error. For eksempel:
```Clojure
(prerrn "Dette er en annen feilmelding")
```
Dette vil skrive ut "Dette er en annen feilmelding" til standard error strømmen.

## Dypdykk:
Å skrive til standard error har vært en vanlig praksis i programmering siden tidlig på 1970-tallet. Det er en del av Unix filosofien om å skille mellom feil og vanlig utdata. Alternativet til å skrive til standard error er å bruke standard output, men det kan føre til forvirring når feilmeldinger blandes inn med vanlig utdata. Det er viktig å merke seg at i Clojure er standard error bare tilgjengelig når programmet kjører i en terminal, og ikke hvis det f.eks. kjøres i et webgrensesnitt.

## Se også:
- [Stack Overflow diskusjon om bruk av standard error i Clojure](https://stackoverflow.com/questions/27475266/how-to-log-to-clojure-stderr)