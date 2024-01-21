---
title:                "Lese kommandolinjeargumenter"
date:                  2024-01-20T17:56:46.481415-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

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