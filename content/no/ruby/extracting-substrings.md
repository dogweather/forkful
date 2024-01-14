---
title:    "Ruby: Utvinning av delstrenger"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Hvorfor

Å eksperimentere med å hente ut substrings fra en streng kan være en nyttig teknikk for å få tilgang til spesifikke deler av en tekst eller variabel. Dette kan være spesielt nyttig for tekstbehandling eller å søke gjennom store datasett.

## Slik gjør du det

```Ruby
# Hent ut en substring basert på start- og sluttposisjon
string = "Hei alle sammen!"
puts string[4, 3] # Output: "alle"

# Hent ut siste bokstav i en streng
string = "Ruby er gøy!"
puts string[-1] # Output: "!"

# Hent ut en substring basert på en indeks
string = "Dette er en tekst"
puts string[6..9] # Output: "er e"

```

Disse eksemplene viser bare noen få måter å hente ut substrings på. Det finnes mange flere metoder og kombinasjoner som kan brukes for å få tilgang til ønsket tekst. Utforsk og prøv deg frem!

## Dypdykk

Å hente ut substrings kan også gjøres ved hjelp av ulike metoder og argumenter, som for eksempel `slice()` og `scan()`. Det er også mulig å bruke regulære uttrykk for å finne og eksakte deler av en streng basert på et mønster. Dette kan være nyttig for å behandle store datamengder og finne spesifikke mønstre.

## Se også

- [Ruby dokumentasjon for å hente ut substrings](https://ruby-doc.org/core-2.7.1/String.html#method-i-5B-5D)
- [Utbredte metoder for å håndtere tekst i Ruby](https://www.rubyguides.com/2015/06/ruby-strings/)
- [Regulære uttrykk i Ruby](https://www.rubyguides.com/2017/02/regular-expressions-ruby/)