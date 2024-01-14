---
title:                "Ruby: Lesing av kommandolinje-argumenter"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å lese kommandolinje-argumenter er en viktig ferdighet for å kunne lage effektive Ruby-programmer. Det er også en viktig del av enhver programmeringsjobb. Ved å lære hvordan kommandolinje-argumenter fungerer, kan du skrive mer avansert og mer tilpassede programmer.

## Hvordan

For å lese kommandolinje-argumenter i Ruby, må du bruke ARGV-metoden. Denne metoden returnerer en matrise med argumentene som ble gitt ved å kjøre programmet. Her er et eksempel:

```ruby
# Program for å lese og skrive ut kommandolinje-argumenter
puts "Følgende kommandolinje-argumenter ble gitt:"
puts ARGV
```

Eksempel på output:

```
$ ruby kommando.rb argument1 argument2
Følgende kommandolinje-argumenter ble gitt:
["argument1", "argument2"]
```

Du kan også bruke ARGV-nummerering for å få en spesifikk del av argumentene. ARGV[0] vil gi den første argumentet, ARGV[1] vil gi det andre argumentet, og så videre. Her er et eksempel på hvordan du kan bruke dette for å lagre argumentene i variabler:

```ruby
# Program for å lagre kommandolinje-argumenter i variabler
argument1 = ARGV[0]
argument2 = ARGV[1]
puts "Argument 1 er: #{argument1}"
puts "Argument 2 er: #{argument2}"
```

Eksempel på output:

```
$ ruby kommando.rb heia norge
Argument 1 er: heia
Argument 2 er: norge
```

## Deep Dive

Når du leser kommandolinje-argumenter, må du være oppmerksom på at argumenter som inneholder mellomrom må omgis med anførselstegn. Dette gjelder også for argumenter som inneholder spesialtegn som "!" og "&". Ellers vil programmet lese argumentene som separate elementer og gi feil output.

Det er også viktig å være klar over at når du leser argumenter, vil ARGV[0] alltid være programnavnet. Derfor må du begynne med å lese ARGV[1] for å få det første argumentet gitt av brukeren.

## Se også

- [Ruby ARGV documentation](https://ruby-doc.org/core-2.7.1/ARGV.html)
- [Ruby ARGF class](https://ruby-doc.org/core-2.7.1/ARGF.html)
- [Ruby Command Line Arguments tutorial](https://www.rubyguides.com/2019/08/ruby-command-line-arguments/)