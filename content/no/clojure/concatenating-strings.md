---
title:                "Sammenslåing av strenger"
html_title:           "Arduino: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Sammenslåing av strenger er en prosess der to eller flere strenger slås sammen til én. Dette er vanlig i programmering for å bygge eller formatere dynamisk genererte meldinger, utføre filbehandlinger, osv.

## Hvordan:

Clojure har en innebygd funksjon kalt `str` som kan brukes til å sette sammen strenger. Her er eksempelet:

```clojure
(str "Hei, " "hvordan " "har " "du " "det?")
```

Denne koden vil produsere:

```clojure
"Hei, hvordan har du det?"
```

Vi kan også konvertere ikke-streng data til en streng med `str` funksjonen:

```clojure
(def alder 25)
(str "Jeg er " alder " år gammel.")
```

Dette vil gi output:

```clojure
"Jeg er 25 år gammel."
```

## Dypdykk

Clojure følger Lisp's filosofi med å ha en minimal kjerne språk og å bygge det opp med biblioteker, noe som inkluderer `str` funksjonen for å sette sammen strenger.

Alternativt kan vi bruke `format` funksjonen, som fungerer nesten som printf eller sprintf funksjonene i andre språk, og gir oss mer kontroll over formatet til den endelige strengen:

```clojure
(format "Hei, %s. Du er %d år gammel." "Ola" 25)
```

Dette vil gi output:

```clojure
"Hei, Ola. Du er 25 år gammel."
```

Under panseret bruker `str` en Java StringBuilder, som håndterer sammenføyning av strenger i en effektiv måte.

## Se også:

For mer informasjon om strenger i Clojure, se disse kildene:

- Clojuredocs eksempler på `str` funksjonen: [clojure.core/str | ClojureDocs](https://clojuredocs.org/clojure.core/str)
- Stack Overflow diskusjon om strengkonkatenering i Clojure: [How to concatenate strings in clojure?](https://stackoverflow.com/questions/6120141/how-to-concatenate-strings-in-clojure)