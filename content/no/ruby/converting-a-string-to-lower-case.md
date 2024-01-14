---
title:    "Ruby: Konvertere en streng til små bokstaver"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor 

Å konvertere en streng til små bokstaver er en nyttig funksjon i Ruby-programmering som kan bidra til å håndtere og manipulere tekst på en bedre måte. Det kan være nyttig når du for eksempel skal sammenligne strenger eller utføre søk i en tekst.

## Hvordan Gjøre Det

For å konvertere en streng til små bokstaver i Ruby, kan du bruke metoden `.downcase`, slik:

```ruby
string = "HELLO WORLD"
puts string.downcase
```

Dette vil gi følgende output: 

```
hello world
```

Som du kan se, blir strengen konvertert til små bokstaver.

Du kan også bruke metoden `.capitalize` til å konvertere bare den første bokstaven i strengen til stor bokstav, og resten til små bokstaver. For eksempel:

```ruby
string = "rUby iS FUN"
puts string.capitalize
```

Dette vil gi følgende output:

```
Ruby is fun
```

Du kan også bruke `.upcase`-metoden for å konvertere en streng til store bokstaver.

## Dykk Dypere

I Ruby blir alle strenger behandlet som objekter og har en rekke forskjellige metoder tilgjengelig for å manipulere dem. Metodene `.downcase`, `.capitalize` og `.upcase` er bare noen få av disse. Det er viktig å forstå at disse metodene ikke endrer i den opprinnelige strengen, men heller returnerer en ny streng med den ønskede endringen.

Det er også verdt å nevne at disse metodene vil fungere på alle typer strenger, uavhengig av om de er lagret i variabler eller om de er harde kodede i koden.

## Se Også

- [String Dokumentasjon i Ruby](https://ruby-doc.org/core/String.html)
- [Arrays i Ruby](https://ruby-doc.org/core/Array.html)
- [Beginners Guide til Ruby](https://www.ruby-lang.org/no/documentation/quickstart/)