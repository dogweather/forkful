---
title:                "Clojure: Uthenting av delstrenger"
simple_title:         "Uthenting av delstrenger"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å utvinne substringer er en nyttig teknikk som gjør det mulig å manipulere tekststrenger på en effektiv måte. Den tillater oss å hente ut deler av en streng basert på spesifikke kriterier, noe som kan være svært nyttig i mange programmeringsscenarier.

## Hvordan

For å utvinne substringer i Clojure, bruker vi funksjonen "subs". Denne funksjonen tar inn en streng som første argument og to tall som representerer start- og sluttposisjonen til substringsen du ønsker å ekstrahere.

```Clojure
(def streng "Dette er en teststreng")
(subs streng 5 7)
```

Outputen av koden ovenfor vil være "er", siden den henter ut delen av strengen fra posisjon 5 (startende fra 0) til posisjon 7, uten å inkludere siste tegn. Dette kan også gjøres ved hjelp av negative tall, hvor -1 vil representere siste tegn i strengen.

```Clojure
(subs streng 5 -4)
```

Outputen av koden over vil være "er en t", som er delen av strengen som starter på posisjon 5 og slutter på posisjon -4.

## Dypdykk

I tillegg til å bruke "subs" funksjonen, kan vi også bruke "substring" funksjonen for å ekstrahere substringer i Clojure. Denne funksjonen tar inn en streng som første argument, og to tall som representerer start- og sluttposisjonen til substringen. Forskjellen her er at "substring" funksjonen inkluderer siste tegn i resultatet.

```Clojure
(substring streng 5 7)
```

Outputen vil være "er ", hvorav det siste mellomrommet er inkludert fra strengen.

## Se også

- [Clojure Docs subs](https://clojuredocs.org/clojure.core/subs)
- [Clojure Docs substring](https://clojuredocs.org/clojure.core/substring)