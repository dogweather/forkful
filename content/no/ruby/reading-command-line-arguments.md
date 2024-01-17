---
title:                "Lesing av kommandolinje-argumenter"
html_title:           "Ruby: Lesing av kommandolinje-argumenter"
simple_title:         "Lesing av kommandolinje-argumenter"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å lese kommandolinje-argumenter er en måte for programmerere å få tilgang til brukerinndata gjennom terminalen. Dette er nyttig for å lage interaktive programmer og gjøre dem mer tilpasningsdyktige for brukeren.

## Hvordan:

Å lese kommandolinje-argumenter er enkelt i Ruby. Alt du trenger å gjøre er å bruke ARGV-variabelen, som lagrer alle argumentene som er gitt når programmet blir kjørt. Her er et eksempel på hvordan du kan skrive ut alle argumentene til terminalen:

```Ruby 
puts ARGV
```
Når du kjører programmet ditt, legger du til argumentene etter filnavnet. For eksempel, hvis filen din heter "program.rb", kan du kjøre den slik:

```Ruby
ruby program.rb arg1 arg2
```
Dette vil skrive ut følgende:

```Ruby
["arg1", "arg2"]
```

Du kan også få tilgang til hvert enkelt argument ved å bruke indekser. ARGV[0] vil være den første parameteren, ARGV[1] vil være den andre og så videre.

## Dypdykk:

Kommandolinje-argumenter har eksistert siden de tidlige dagene av programmering, og er fortsatt et viktig konsept i dag. En annen måte å lese brukerinndata på er gjennom standard inndata streams, men kommandolinje-argumenter er ofte foretrukket fordi de er enklere å lese og behandle.

Det er flere alternativer til å bruke ARGV i Ruby. En måte er å bruke det mer spesialiserte optparse-biblioteket, som lar deg definere hvilke argumenter som skal være tilgjengelige og få tilgang til dem på en mer strukturert måte.

På et mer teknisk nivå, når du kjører et Ruby-program med kommandolinje-argumenter, blir alle argumentene satt inn i et array som ARGV-variabelen refererer til. Dette arrayet er tilgjengelig for å bli behandlet av programmet ditt.

## Se også:

- [Ruby ARGV documentation](https://ruby-doc.org/core-2.7.0/ARGV.html)
- [Optparse documentation](https://ruby-doc.org/stdlib-2.7.0/libdoc/optparse/rdoc/OptionParser.html)