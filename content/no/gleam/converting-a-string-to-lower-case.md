---
title:                "Oversette en streng til små bokstaver."
html_title:           "Gleam: Oversette en streng til små bokstaver."
simple_title:         "Oversette en streng til små bokstaver."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en streng til små bokstaver er en vanlig operasjon i programmering. Det kan være nyttig å gjøre dette når man ønsker å sammenligne strenger, eller når man trenger å formatere en streng til en spesifikk stil.

Å gjøre dette i Gleam er en enkel prosess som kan spare deg for mye tid og hodebry. Her vil vi vise deg hvordan du enkelt kan konvertere en streng til små bokstaver ved hjelp av Gleam.

## Hvordan

```Gleam
let string = "Hallo Gleam!"
let lowercased_string = String.to_lower(string)

// Output: hallo gleam!
```

For å konvertere en streng til små bokstaver i Gleam, bruker vi funksjonen `String.to_lower`. Denne funksjonen tar inn en streng som parameter og returnerer en ny streng med alle bokstavene i små bokstaver.

Ved hjelp av variabelen `lowercased_string` kan vi nå bruke denne konverterte strengen videre i koden vår. Det er viktig å merke seg at denne funksjonen ikke endrer den originale strengen, men heller returnerer en ny konvertert versjon.

En ting å være oppmerksom på er at denne funksjonen kun konverterer bokstaver til små bokstaver. Spesialtegn eller tall vil fortsatt være uendret i den konverterte strengen.

## Deep Dive

Bak kulissene bruker Gleam Unicode-standard for å konvertere strenger til små bokstaver. Dette betyr at det vil fungere på alle språk som støttes av Unicode, inkludert norske bokstaver som æ, ø og å.

En annen ting å merke seg er at `String.to_lower` ikke er begrenset til kun å konvertere engelske bokstaver. Dette betyr at du også kan bruke den til å konvertere bokstaver fra andre språk.

Det finnes også andre metoder for å konvertere bokstaver til forskjellige stiler, for eksempel `String.to_upper` for å konvertere til store bokstaver og `String.to_titlecase` for å konvertere til tittelcase.

## Se Også

- [Gleam dokumentasjon](https://gleam.run/documentation)
- [Unicode offisiell nettside](https://unicode.org/)
- [Gleam på GitHub](https://github.com/gleam-lang/gleam)