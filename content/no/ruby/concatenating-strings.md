---
title:                "Sammenstilling av tekststrenger"
html_title:           "Ruby: Sammenstilling av tekststrenger"
simple_title:         "Sammenstilling av tekststrenger"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Konkatenering av strenger er en metode for å kombinere to eller flere strenger i én og skape en ny streng. Dette er nyttig for programmerere når de trenger å lage en dynamisk streng som er satt sammen av forskjellige elementer.

## Hvordan:
Her er et eksempel på hvordan man kan konkatenerere strenger i Ruby:
```Ruby
fornavn = "Ola"
etternavn = "Nordmann"
fullt_navn = fornavn + " " + etternavn
puts fullt_navn
```

Dette vil gi følgende output:
```
Ola Nordmann
```

En annen måte å konkatenerere strenger på i Ruby er ved hjelp av interpolasjon:
```Ruby
fornavn = "Kari"
alder = 30
puts "#{fornavn} er #{alder} år gammel"
```

Dette vil gi følgende output:
```
Kari er 30 år gammel
```

## Dypdykk:
Historisk sett, ble konkatenering ofte brukt for å sette sammen deler av en tekststreng eller formatere en utskrift. I dag, med fremveksten av mer komplekse programmeringsspråk og funksjonaliteter, er det flere måter å skape dynamiske strenger på, som f.eks. ved hjelp av metoden `sprintf` eller ved å bruke arrays.

Det er flere alternativer til konkatenering av strenger, som f.eks. å bruke templating-verktøy som ERB eller HAML. Disse verktøyene tillater å skrive dynamiske HTML- og XML-dokumenter ved hjelp av en kombinasjon av kode og tekst.

Når du konkatenerer strenger i Ruby, blir en ny streng opprettet, da Ruby-strenger er immutable (kan ikke endres). Dette betyr at hver gang du konkatenerer, vil det opprettes en ny streng i minnet.

## Se også:
- [Ruby kodetilbydere](https://www.rubyguides.com/2018/03/ruby-string-manipulation/)
- [Metoder for å konkatenerere strenger i Ruby](https://www.javalobby.org/article/algorithms-for-concatenating-strings-in-ruby/)