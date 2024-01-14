---
title:                "Gleam: Utdrag av delstrenger"
simple_title:         "Utdrag av delstrenger"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor vil noen ønske å trekke ut substringer i Gleam-programmering? Å extrahere substringer er et nyttig verktøy som kan brukes for å manipulere strenger på en mer effektiv måte. Det kan også være nyttig for å analysere og håndtere data på en mer nøyaktig måte.

## Hvordan

For å ekstrahere substringer i Gleam, kan du bruke funksjonen `substring()` som tar inn tre parametere: en streng, en startindeks og en sluttindeks. Her er et eksempel på hvordan du kan bruker denne funksjonen:

```Gleam
let str = "Hei, dette er en Gleam bloggpost."
let sub = substring(str, 5, 12)
```

I dette eksempelet vil `sub` variabelen inneholde ordet "dette". `substring()`-funksjonen starter å telle indekser fra 0, så 5 representerer starten på ordet "dette", og 12 representerer slutten av ordet.

Outputen av dette eksempelet vil være `dette er`, siden funksjonen inkluderer både start- og sluttindeks i det ekstraherte substringet.

## Deep Dive

Det er verdt å merke seg at `substring()`-funksjonen i Gleam også kan brukes med negative indekser. Dette betyr at du kan trekke ut substringer fra slutten av en streng i stedet for begynnelsen. Her er et eksempel på hvordan du kan trekke ut de to siste ordene fra en streng:

```Gleam
let str = "Dette er den siste strengen."
let sub = substring(str, -2, -1)
```

Outputen vil da være `siste strengen`.

En annen nyttig funksjon i forbindelse med substringer er `split()`-funksjonen. Denne funksjonen tar inn en streng og et skillemerke som parametere, og returnerer en liste av substringer som er separert av skilletegn. Her er et eksempel:

```Gleam
let str = "Apple, Orange, Banana, Mango"
let list = split(str, ", ")
```

Outputen vil da være en liste med fire elementer: "Apple", "Orange", "Banana" og "Mango".

## Se også

- [Offisiell Gleam dokumentasjon om substringer](https://gleam.run/documentation/stdlib/string.html#functions)
- [En guide til Gleam for nybegynnere (på norsk)](https://medium.com/@danielschukert/an-introduction-to-gleam-for-beginners-e96f788225e2)
- [En introduksjon til strenger i Gleam (på norsk)](https://medium.com/@danielschukert/a-beginners-guide-to-strings-in-gleam-norwegian-c59aacc87e3f)