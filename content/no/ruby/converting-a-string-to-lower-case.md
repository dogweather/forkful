---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Arduino: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å konvertere en streng til små bokstaver er når du endrer alle tegnene i en tekststreng til små bokstaver. Programmerere gjør dette for å standardisere data, og det er spesielt nyttig i situasjoner hvor tekstrelevante sammenligninger og søk skal være "case-insensitive".

## Hvordan:
Her er kjappe eksempler på kodestubber - la oss si vi har strengen "Hei, Verden!" og vi ønsker å konvertere den til små bokstaver.

```Ruby
# Vi har vår opprinnelige streng
original_string = "Hei, Verden!"

# Vi bruker downcase-metoden
lowercase_string = original_string.downcase

# La oss skrive ut resultatet
puts lowercase_string

# Output vil være: "hei, verden!"
```

## Dyp Dykk:

Ruby's `.downcase` metoden ble opprinnelig introdusert i språket for å hjelpe med tekstbehandling og manipulering. Det er en av flere innebygde metoder som Ruby tilbyr for å operere på strenger. Alternativer kan variere avhengig av situasjon, og kan inkludere bruken av `.upcase` for å konvertere en streng til store bokstaver, eller `.capitalize` for å gjøre den første bokstaven i strengen stor.

Implementeringen av `.downcase` i Ruby bruker konseptet av ASCII-tabellen og endrer hvert tegn deretter. Dette er en svært effektiv metode som kan håndtere store strenger uten noen betydelig ytelsespåvirkning.

## Se Også:

- Ruby Dokumentasjon: [String#downcase](https://ruby-doc.org/core-2.7.1/String.html#method-i-downcase)
- Stack Overflow: [Hvordan konvertere en streng til små bokstaver i Ruby](https://stackoverflow.com/questions/5030631/how-do-i-convert-a-string-to-lower-case-in-ruby)
- RubyGuides: [Spesialtegn og Strengmanipulering](https://www.rubyguides.com/2018/01/ruby-string-methods/)