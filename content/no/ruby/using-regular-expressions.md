---
title:                "Ruby: Bruk av regulære uttrykk"
simple_title:         "Bruk av regulære uttrykk"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du er i utviklingsverdenen, har du sikkert hørt om regular expressions. Men hvorfor skal du bruke dem? Regular expressions er et kraftig verktøy som lar deg søke og manipulere tekst basert på mønstre. Det kan være nyttig for å finne og erstatte tekst, validere inndata og mye mer. Det er også et utbredt verktøy som brukes i mange programmeringsspråk, inkludert Ruby.

## Hvordan

For å bruke regular expressions i Ruby, trenger du først å inkludere `regexp` biblioteket ved å skrive `require 'regexp'` øverst i filen din. Deretter kan du skrive et mønster ved å bruke `/` rundt det, for eksempel `/hello world/`, som vil matche teksten "hello world" i en streng. Du kan også bruke forskjellige spesielle tegn, som `.` for å matche ethvert tegn, `+` for å matche ett eller flere forekomster av det foregående tegnet, og `*` for å matche null eller flere forekomster. La oss se på et eksempel:

```Ruby
text = "Hello world, this is a blog post written in Norwegian!"
pattern = /H.*?n[wd]/
puts text.gsub(pattern, "Hallo")
```

I dette eksemplet vil vi erstatte alle ord som starter med `H` og slutter med `n` eller `w` med "Hallo", slik at vi får ut "Hallo, Hallo, Hallo Hallo Hallo Halo!".

## Dypdykk

Regulære uttrykk kan være kompliserte og kraftige, og det er umulig å dekke alt i en enkelt blogginnlegg. Men det er noen få ting du bør vite når du bruker dem. Først, sørg for å bruke nøyaktig matche hvis du vil at uttrykket ditt skal matche hele strenger og ikke bare deler av det. For eksempel hvis vi endrer mønsteret vårt i eksemplet ovenfor til `/H.*n[wd]/`, vil vi få ut "Hallo world, this is a blog post written in Norwegian!". Merk at "Hallo hello Hallo Hallo Hallo Halo" er ikke en ønsket utgang.

En annen ting å huske på er at regular expressions er case sensitive. Det betyr at `/hello/` ikke vil matche "Hello", så du må ta hensyn til store og små bokstaver når du lager dine mønstre.

Det er også lurt å være forsiktig når du bruker spesielle tegn som `.` og `+`. De kan være veldig nyttige, men også føre til uønskede resultater hvis du ikke bruker dem riktig.

## Se også

- [Rubys dokumentasjon om regular expressions](https://ruby-doc.org/core-2.3.1/Regexp.html)
- [En interaktiv tutorial om regular expressions i Ruby](https://www.codecademy.com/en/courses/ruby-beginner-en-NFCZ7/0/1)
- [En praktisk guide for regular expressions i Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)