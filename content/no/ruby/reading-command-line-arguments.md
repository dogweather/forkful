---
title:                "Lese kommandolinjeargumenter"
html_title:           "Ruby: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lese kommandolinjeargumenter er en viktig del av Ruby-programmering, spesielt når du ønsker å lage interaktive programmer som krever innspill fra brukeren. Det kan også være nyttig for å gi spesifikk funksjonalitet basert på argumentene som er passert inn i programmet.

## Hvordan

Det er enkelt å lese kommandolinjeargumenter i Ruby ved å bruke standardbiblioteket `ARGV`. Her er et eksempel på hvordan du kan ta inn to tall som argumenter og utføre en enkel matematisk operasjon:

```Ruby
number1 = ARGV[0].to_i
number2 = ARGV[1].to_i

puts "Summen av #{number1} og #{number2} er #{number1 + number2}"
```

Når du kjører dette programmet med følgende kommandolinje: `ruby program.rb 3 5`, vil du få følgende output:

```
Summen av 3 og 5 er 8
```

Som du ser, bruker vi `ARGV` for å få tak i argumentene som er passert inn ved bruk av indeksering. Vi konverterer også argumentene til heltall ved hjelp av `.to_i`-metoden for å kunne utføre matematiske operasjoner.

## Dypdykk

Ved å dykke dypere inn i `ARGV` kan du også legge merke til at den tar inn alle argumentene som en array, uansett hvor mange det er. Det betyr at du kan ta inn en variabel mengde av argumenter og behandle dem som du ønsker. Her er et eksempel på hvordan du kan skrive ut alle argumentene som blir passert inn som strenger:

```Ruby
ARGV.each do |argument|
  puts "Argument: #{argument}"
end
```

Når du kjører dette programmet med følgende kommandolinje: `ruby program.rb hello world`, vil du få følgende output:

```
Argument: hello
Argument: world
```

Dette gir deg muligheten til å lage programmer som kan tilpasses basert på brukerens input.

## Se også

- [Ruby Standardbiblioteket](https://ruby-doc.org/stdlib-2.7.0/libdoc/)
- [Kommandolinje argumenter i Ruby](https://www.rubyguides.com/2019/05/ruby-command-line-arguments/)