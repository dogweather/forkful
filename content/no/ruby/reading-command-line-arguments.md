---
title:                "Ruby: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Kommandolinjeargumenter er en viktig del av Ruby-programmering, da de lar deg ta input fra brukeren direkte fra kommandolinjen. Dette gjør det mulig å lage programvare som er mer interaktiv og brukervennlig.

## Hvordan

 For å lese kommandolinjeargumenter i Ruby, kan du bruke ARGV-objektet som inneholder alle argumentene som ble gitt ved kjøring av programmet. Her er et eksempel på hvordan du kan lese og skrive ut disse argumentene:

```ruby
# Les argumentene og lagre dem i en variabel
argumenter = ARGV

# Skriv ut hver av argumentene
argumenter.each do |argument|
  puts argument
end
```

Når du kjører dette programmet og gir det noen argumenter, vil du se følgende output:

```
ruby lese_argumenter.rb argument1 argument2 argument3
```

```
argument1
argument2
argument3
```

Du kan også bruke ARGV-objektet til å lese et spesifikt argument basert på index, for eksempel `ARGV[0]` for det første argumentet.

## Dypdykk

Det er også mulig å håndtere ulike situasjoner som kan oppstå når man leser kommandolinjeargumenter. For eksempel kan du sjekke om et argument er gitt ved å bruke `.include?` metoden eller sjekke antall argumenter som er gitt med `.length` metoden. Du kan også konvertere argumentene til forskjellige typer, som for eksempel tall eller symboler.

En annen nyttig funksjon å vite om er `OptionParser` som lar deg definere og behandle argumenter på en mer strukturert måte. Dette kan være nyttig når du jobber med større og mer komplekse programmer.

## Se Også

- [Ruby Dokumentasjon om ARGV](https://ruby-doc.org/core-3.0.0/ARGF.html)
- [Ruby Dokumentasjon om OptionParser](https://ruby-doc.org/stdlib-2.7.3/libdoc/optparse/rdoc/OptionParser.html)
- [Kommandolinjeargumenter i Ruby av ThoughtBot](https://thoughtbot.com/blog/ruby-command-line-arguments)