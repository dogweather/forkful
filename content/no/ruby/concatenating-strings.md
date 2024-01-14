---
title:                "Ruby: Sammenslåing av tekststrenger"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å slå sammen strenger er en viktig del av Ruby-programmering og er nyttig når du vil kombinere forskjellige tekststrenger til en enkelt streng. Dette kan være nyttig for å lage dynamiske tekster, til å vise resultatene av variabler eller for å formatere utdataen på en spesiell måte. Ved å lære hvordan du slår sammen strenger, kan du gjøre kodene dine mer effektive og funksjonelle.

## Hvordan

Koden for å slå sammen strenger i Ruby er veldig enkel. Du kan bruke "+" operatøren for å slå sammen to strenger, eller du kan bruke "<<" symbolet for å legge til en streng i enden av en annen.

La oss se på et eksempel:

```Ruby
navn = "Jørgen"
alder = 25
puts "Hei, mitt navn er " + navn + " og jeg er " + alder.to_s + " år gammel."
```

I dette eksempelet benytter vi oss av både "+" operatøren og "<<" symbolet for å slå sammen flere strenger. Vi bruker også ".to_s" for å konvertere variablene til strenger, siden vi ikke kan slå sammen strenger og tall direkte.

Når du kjører dette eksempelet, vil du få følgende utdata:

```
Hei, mitt navn er Jørgen og jeg er 25 år gammel.
```

## Dypdykk

I tillegg til å bruke "+" operatøren og "<<" symbolet, kan du også bruke metoden ".concat" for å slå sammen strenger i Ruby. Denne metoden legger til en streng i enden av en annen, og endrer strengen den blir brukt på.

La oss se på et eksempel:

```Ruby
sangen = "Jeg elsker "
sangen.concat("å synge.")
puts sangen
```

I dette tilfellet vil strengen "Jeg elsker " bli endret til "Jeg elsker å synge." ved hjelp av ".concat" metoden.

Du kan også bruke interpolasjon for å sette inn variabler i en streng. Dette gjøres ved å bruke "#" før variabelnavnet inne i en streng. La oss se på et eksempel:

```Ruby
navn = "Oda"
puts "Hei, mitt navn er #{navn} og jeg elsker å kode."
```

Dette vil gi følgende utdata:

```
Hei, mitt navn er Oda og jeg elsker å kode.
```

## Se Også

For å lære mer om strenger i Ruby og deres bruksområder, kan du besøke disse ressursene:

- [Offisiell Ruby dokumentasjon for strenger](https://ruby-doc.org/core-2.7.1/String.html)
- [Enkel Guide til Ruby - Strenger](https://www.tutorialspoint.com/ruby/ruby_strings.htm)
- [Ruby Strenger - En Omfattende Guide](https://www.rubyguides.com/2015/05/ruby-strings/)