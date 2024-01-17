---
title:                "Utvinning av substringer"
html_title:           "Ruby: Utvinning av substringer"
simple_title:         "Utvinning av substringer"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?
Ekstrahering av substrings er en måte å plukke ut en del av en tekststreng i programmering. Dette kan være nyttig når man trenger å hente ut spesifikke deler av en tekst for å bruke i videre beregninger eller manipulasjoner. Det er en vanlig oppgave blant programmerere som jobber med tekstbehandling og dataanalyse.

## Hvordan:
Ekstrahering av substrings kan gjøres veldig enkelt i Ruby ved hjelp av en innebygd funksjon kalt "slice", som tar to parametere - start- og sluttposisjonen til den ønskede delen av tekststrengen. Her er et eksempel:

```Ruby
my_string = "Dette er en tekststreng"
substring = my_string.slice(8, 2) # Starter på posisjon 8 og tar 2 tegn
puts substring # Skriver ut "en"
```

Man kan også bruke "[]" og angi posisjoner med tall eller et "range" (et område av tall). Her er et eksempel på å hente ut en del av en tekststreng basert på området av tall:

```Ruby
my_string = "Dette er en tekststreng"
substring = my_string[8..10] # Henter alt fra posisjon 8 til og med 10
puts substring # Skriver ut "en "
```

## Dypdykk:
Å kunne ekstrahere substrings har vært en viktig del av programmering siden de tidlige dagene med tekstbaserte programmer. Før i tiden måtte man bruke "substring" -funksjoner som var spesifikke for hver type programmeringsspråk. I dagens moderne språk som Ruby, er dette implementert som en standard funksjon og gjør det mye enklere.

En alternativ metode til å ekstrahere substrings er ved hjelp av Regular Expression (regex), et kraftig verktøy for å finne og manipulere tekst. Dette kan være nyttig hvis man trenger å hente ut mer komplekse deler av en tekststreng.

Når det kommer til implementeringsdetaljer, er "slice" -funksjonen i Ruby veldig effektiv siden den bare kopierer den delen av tekststrengen som blir ekstrahert i stedet for å lage en ny kopi av hele teksten.

## Se også:
- [Ruby dokumentasjon om slice](https://ruby-doc.org/core-2.7.2/String.html#method-i-slice)
- [Regular Expression tutorial](https://www.regular-expressions.info/tutorial.html)