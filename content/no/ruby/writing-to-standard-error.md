---
title:                "Skriver til standardfeil"
html_title:           "Ruby: Skriver til standardfeil"
simple_title:         "Skriver til standardfeil"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?
Å skrive til standardfeil betyr å sende feilmeldinger til en spesifikk type output i Ruby-programmering. Dette er nyttig fordi det skiller feil- og informasjonsmeldinger, og gjør det enklere å finne og løse problemer i koden.

## Hvordan gjør man det:
For å skrive til standardfeil i Ruby, bruker du bare ```$stderr.puts()``` funksjonen i koden din. Dette vil skrive ut meldingene dine i standardfeil-output, i stedet for standard output.

```ruby
$stderr.puts("Dette er en feilmelding")
```

Dette vil gi følgende output:
```
Dette er en feilmelding
```

## Hva ligger under overflaten:
Skriving til standardfeil er en vanlig praksis i programmering og finnes også i andre programmeringsspråk som C og Java. Alternativet til å skrive til standardfeil er å bruke ```$stdout.puts()```, som vil skrive til standard output.

Når du skriver til standardfeil, bruker du egentlig en funksjon som heter ```$stderr.write()```, som skriver meldingen uten å legge til linjeskift. Dette kan være nyttig når du vil kontrollere hvor linjeskift skal være i koden din.

## Se også:
- [Ruby dokumentasjon for $stderr](https://ruby-doc.org/core-2.6.3/IO.html#method-c-fs)
- [Stack Overflow-innlegg om skriving til standardfeil](https://stackoverflow.com/questions/2612369/when-to-use-stderr-instead-of-stdout-in-bash-scripts)