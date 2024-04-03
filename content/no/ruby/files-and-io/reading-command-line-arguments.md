---
date: 2024-01-20 17:56:46.481415-07:00
description: "Kommandolinjeargumenter lar deg sende data direkte til et Ruby-program\
  \ n\xE5r du kj\xF8rer det. Programmere bruker dette for \xE5 gj\xF8re programmene\
  \ fleksible og\u2026"
lastmod: '2024-03-13T22:44:41.348665-06:00'
model: gpt-4-1106-preview
summary: "Kommandolinjeargumenter lar deg sende data direkte til et Ruby-program n\xE5\
  r du kj\xF8rer det."
title: Lese kommandolinjeargumenter
weight: 23
---

## Hva & Hvorfor?
Kommandolinjeargumenter lar deg sende data direkte til et Ruby-program når du kjører det. Programmere bruker dette for å gjøre programmene fleksible og tilpasse kjøringen uten å endre koden.

## Hvordan:
Her er et enkelt Ruby-script som leser argumenter fra kommandolinjen:

```Ruby
# kommandolinje_arg.rb
argumenter = ARGV
puts "Du har gitt meg #{argumenter.length} argument(er):"
argumenter.each_with_index do |arg, index|
  puts "Argument #{index + 1}: #{arg}"
end
```
Kjør scriptet med noen argumenter:
```
$ ruby kommandolinje_arg.rb Hei Verden!
Du har gitt meg 2 argument(er):
Argument 1: Hei
Argument 2: Verden!
```

## Dypdykk
Tilbake på 90-tallet, da Ruby først kom ut, var det viktig å behandle inputt fra kommandolinjen effektivt, spesielt for script og server-side programmer. Alternativer til `ARGV` inkluderer bruk av `gets` for å lese interaktiv input eller miljøvariabler. Implementasjonsdetaljer: `ARGV` er et globalt array som Ruby tolker fyller med kommandolinjeargumentene før skriptet kjøres. I moderne Ruby-versjoner kan du også bruke `OptionParser` for mer komplekse behov.

## Se også
- Ruby's dokumentasjon om ARGV: https://www.ruby-lang.org/en/documentation/quickstart/4/
- `OptionParser` klasse dokumentasjon: https://ruby-doc.org/stdlib-2.6.1/libdoc/optparse/rdoc/OptionParser.html
- En guide til kommandolinjeprogrammering i Ruby: https://www.jstorimer.com/blogs/workingwithcode/7766119-building-awesome-command-line-programs-in-ruby
