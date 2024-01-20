---
title:                "Sammenslåing av strenger"
html_title:           "Arduino: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sammenslåing av strenger ('String concatenation') er en metode i programmering for å kombinere to eller flere tekststrenger til én enkelt tekststreng. Dette er nyttig for å organisere og formatere utdata, sette sammen SQL-spørringer, og mange andre ting.

## Hvordan gjør man det:
Her er noen forskjellige måter å sette sammen strenger på i Ruby. Prøv dem selv!

```Ruby
# Bruke '+' operatøren:
hilsen = 'Hei'
navn = 'Ola'
hilsen_navn = hilsen + ', ' + navn + '!'
puts hilsen_navn
# Output: 'Hei, Ola!'

# Bruke '<<' operatøren:
hilsen = 'Hei'
navn = 'Ola'
hilsen << ', ' << navn << '!'
puts hilsen
# Output: 'Hei, Ola!'

# Bruke sprintf:
navn = 'Ola'
hilsen_navn = sprintf('Hei, %s!', navn)
puts hilsen_navn
# Output: 'Hei, Ola!'
```

## Dypdykk
Historisk sett, Ruby har alltid støttet flere metoder for å sette sammen strenger. Når det gjelder effektivitet, er `+` og `<<` omtrent like, men sjekk alltid dokumentasjonen og test koden din.

Det finnes alternativer til sammenslåing av strenger i Ruby. Du kan for eksempel bruke en 'String Interpolation'. Denne metoden gir mulighet for å sette inn verdier direkte i en streng: 

```Ruby
navn = 'Ola'
hilsen_navn = "Hei, #{navn}!"
puts hilsen_navn
# Output: 'Hei, Ola!'
```

Husk, `+` og `sprintf` returnerer en ny streng, mens `<<` endrer den originale strengen.

## Se også
* Detaljert info om [String concatenation](https://en.wikipedia.org/wiki/Concatenation#In_programming_languages) på Wikipedia.
* Ruby dokumentasjon for [String](https://ruby-doc.org/core-2.7.0/String.html) klasse.
* Diskusjon om effektivitet av ulike [metoder](https://stackoverflow.com/questions/10076579/string-concatenation-vs-string-interpolation-in-ruby) for å sette sammen strenger i Ruby.