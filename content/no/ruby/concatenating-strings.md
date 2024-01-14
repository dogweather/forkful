---
title:                "Ruby: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kombinere strenger (concatenate strings) er en viktig del av programmering som lar deg sette sammen flere tekststrenger til en enkelt streng. Dette er nyttig for å lage dynamiske tekster eller formater for å presentere data på en lesbar måte.

## Hvordan

Man kan enkelt kombinere strenger i Ruby ved å bruke "+" -operatøren. Se eksempelet nedenfor:

```Ruby
navn = "Marit"
alder = 25

puts "Hei, mitt navn er " + navn + ", og jeg er " + alder.to_s + " år gammel."
```

Dette vil resultere i følgende utskrift:

```ruby
Hei, mitt navn er Marit, og jeg er 25 år gammel.
```

Vi kan også bruke string-interpolasjon ved hjelp av "#{}" -syntaxen. Dette gjør det mulig å inkludere variabler rett inn i teksten uten å måtte konvertere dem til strenger. Eksempelet nedenfor viser hvordan man kan bruke string-interpolasjon for å oppnå det samme resultatet som det forrige eksempelet:

```Ruby
navn = "Marit"
alder = 25

puts "Hei, mitt navn er #{navn}, og jeg er #{alder} år gammel."
```

## Dypdykk

I tillegg til å kombinere strenger ved hjelp av "+", kan vi også bruke metoden "concat" for å oppnå samme effekt. Det er viktig å merke seg at "concat" endrer den opprinnelige strengen, mens "+"-operatøren lager en helt ny streng.

En annen viktig ting å huske på er at det ikke er begrensninger på hvor mange strenger man kan kombinere. Man kan enkelt legge til flere variabler og tekststrenger ved hjelp av "+" eller string-interpolasjon.

## Se også

- Offisiell dokumentasjon for string manipulation <https://ruby-doc.org/core-3.0.2/String.html>
- Enkel guide til Ruby <https://www.ruby-lang.org/no/documentation/quickstart/>
- Mer avansert guide til Ruby <https://www.tutorialspoint.com/ruby/>