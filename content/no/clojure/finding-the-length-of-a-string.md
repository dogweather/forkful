---
title:                "Å finne lengden på en streng"
html_title:           "Clojure: Å finne lengden på en streng"
simple_title:         "Å finne lengden på en streng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

#🔑 Finn Lengden av En Streng

## Hva & Hvorfor?

For å finne lengden av en streng i Clojure, kan du bruke funksjonen `(count)`. Dette kan være nyttig når du trenger å behandle strenger på en bestemt måte, for eksempel å begrense antall tegn som skal vises i en tekstboks eller å kontrollere gyldigheten av et passord med en minimumslengde.

## Hvordan:

```Clojure
(count "Hei, dette er en streng!")
;; Output: 26
```

```Clojure
(count "12345")
;; Output: 5
```

## Dypdykk

En lengdefunksjon har eksistert i Clojure siden begynnelsen, men det er viktig å merke seg at null-verdier ikke teller som en karakter. Alternativt kan du bruke funksjonen `(str)`, som returnerer strengrepresentasjonen av et objekt og deretter telle lengden på denne strengen. Implementasjonen av `(count)` funksjonen bruker Java-metoden `length()` som returnerer antall tegn i en streng.

## Se også:

Clojure Dokumentasjon: https://clojure.org/api/cheatsheet

Java String-metoder: https://www.w3schools.com/java/java_ref_string.asp