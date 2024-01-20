---
title:                "Å bruke regulære uttrykk"
html_title:           "Ruby: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?

Regular expressions er en måte å søke og manipulere tekst på i programmering. Det er en nyttig verktøy for å søke etter spesifikke mønstre, som for eksempel e-postadresser eller telefonnummer, i store tekstfiler.

Programmerere bruker regelmessige uttrykk for å effektivt behandle og transformere data, spesielt når det kommer til store tekstmengder. Det gjør at de kan automatisk finne og endre tekst på et bestemt mønster, noe som sparer tid og gjør kodingen mer nøyaktig.

## Slik gjør du:

For å bruke regulære uttrykk i Ruby må du først opprette et uttrykk ved å bruke et spesielt syntaks:

```Ruby
regex = /mønster/
```

Deretter kan du bruke ulike metoder for å søke etter og manipulere tekst basert på dette mønsteret. For eksempel kan du bruke `match` for å finne alle forekomster av mønsteret i en tekststreng:

```Ruby
text = "Jeg liker å spise epler og bananer"
mønster = /epler/

puts text.match(mønster)

# => epler
```

Her blir bare "epler" returnert siden det er det eneste mønsteret i tekststrengen. Du kan også bruke `sub` for å erstatte en tekst som matcher mønsteret med en annen tekst:

```Ruby
text = "Jeg liker å spise epler og bananer"
mønster = /epler/

puts text.sub(mønster, "appelsiner")

# => Jeg liker å spise appelsiner og bananer
```

## Dypdykk:

Bruken av regulære uttrykk går tilbake til 1950-tallet da det ble utviklet innenfor matematikk og lingvistikk. Det har blitt en viktig del av programmering, spesielt for å arbeide med tekstdata.

Det finnes alternative måter å behandle tekst på i Ruby, som for eksempel `String#scan` metoden. Men regulære uttrykk er ofte foretrukket fordi det tillater mer komplekse og nøyaktige søk. Det er også en del av programmeringskunnskap som er nyttig for å forstå og implementere.

## Se også:

For mer informasjon om bruk av regulære uttrykk i Ruby, sjekk ut disse ressursene:

- [Ruby Regexp dokumentasjon](https://ruby-doc.org/core/Regexp.html)
- [Pragmatic Programing: Chapter 7 - Regular Expressions](https://pragprog.com/book/btlang/best-of-ruby-quiz)