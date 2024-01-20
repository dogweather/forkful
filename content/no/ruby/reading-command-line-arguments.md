---
title:                "Lese kommandolinjeargumenter"
html_title:           "Arduino: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Behandle kommandolinje-argumenter med Ruby

## Hva & Hvorfor?

Kommando-linje-argumenter er inputs som brukes til å styre programfløden, gitt ved program-start. Programmers bruker dem for å tilpasse programoppførsel uten endringer i koden.

## Hvordan?

Ruby gjør behandling av kommandolinje-argumenter enkelt med `ARGV` array. Den lagrer argumentene som strenger. Her er hvordan:

```Ruby
# Lage et program som gjentar de gitte argumentene
ARGV.each do |argument|
  puts "Gitt argument: #{argument}"
end
```
Hvis du kjører dette skriptet som `ruby script.rb Hei Verden`, vil du få:

```Bash
Gitt argument: Hei
Gitt argument: Verden
```

## Deep Dive

La oss se litt dypere inn i denne egenskapen av Ruby.

1. **Historisk kontekst**: Fra Unix's tidlige dager har det vært tradisjon å gi innstillinger og data til programmer via kommandolinje-argumentene.
2. **Alternativene**: Ruby har mange bibliotek som `optparse` for å hjelpe deg med å håndtere mer komplekse argumenter og flagg.
3. **Implementasjonsdetaljer**: `ARGV` er bare en global array. Dere kan manipulere den på samme måte som alle andre arrays i Ruby. ARGV[0] inneholder det første argumentet, ARGV[1] inneholder det andre, og så videre.

## Se også

Her er noen hjelpsomme kilder som du kan se for å lære mer:

- [Ruby docs om ARGV](https://ruby-doc.org/core-2.1.4/ARGV.html)
- [Offisielle Ruby ARGV eksempler](http://www.ruby-doc.org/core-2.1.5/doc/syntax/command_line_rdoc.html)
- [Kommandolinje-argumenter-guide fra RubyGarage](https://rubygarage.org/blog/ruby-command-line-arguments)