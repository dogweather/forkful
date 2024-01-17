---
title:                "Slette tegn som passer mønster"
html_title:           "Gleam: Slette tegn som passer mønster"
simple_title:         "Slette tegn som passer mønster"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Sletting av tegn som matcher et mønster er en nyttig funksjon i programmering som lar deg enkelt fjerne uønskede tegn fra en tekststreng. Dette brukes ofte til å rengjøre og formatere data, og det sparer programmerere tid og arbeid.

# Hvordan:
```
Gleam.string.delete_pattern("Hei, dette er en tekst", ",") // Output: "Hei dette er en tekst"
Gleam.string.delete_pattern("1234567890", "[0-9]") // Output: ""
```

Her ser vi eksempler på hvordan vi kan bruke funksjonen `delete_pattern` i Gleam. Vi gir funksjonen en tekststreng som første argument, og et mønster som skal matches og slettes som det andre argumentet. Resultatet vil være en ny tekst med alle tegn som matcher mønsteret fjernet.

# Dypdykk
Sletting av tegn basert på et mønster har vært en vanlig funksjon i programmering siden begynnelsen av datamaskinens tidsalder. Dette er fordi det er en effektiv måte å håndtere og manipulere data på. Alternativene til å slette tegn basert på et mønster inkluderer manuell looping gjennom tegnene, noe som kan være både tidkrevende og feil-introduserende.

Gleam implementerer sletting av tegn som matcher et mønster ved å bruke regulære uttrykk, som er et kraftig verktøy for å matche mønstre i tekststrenger. Dette gjør det mulig for Gleam å håndtere mer komplekse slettekrav, som å slette alle tall eller alle bokstaver.

# Se også
For mer informasjon om hvordan Gleam håndterer tekststrenger og bruk av regulære uttrykk, kan du se dokumentasjonen på deres offisielle nettside: https://gleam.run/documentation/text.html#regular-expressions