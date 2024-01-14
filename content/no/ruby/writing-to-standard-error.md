---
title:                "Ruby: Skriver til standardfeil"
simple_title:         "Skriver til standardfeil"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor du bør bruke standard error når du koder

Mange nye Ruby-utviklere lurer på hvorfor de trenger å bruke standard error når de koder. Svaret er enkelt: det hjelper deg med å finne og løse feil i koden din. Når du skriver til standard error, kan du spore feil på en mer effektiv måte og få bedre forståelse av hva som skjer i programmet ditt.

## Hvordan du kan skrive til standard error i Ruby

Å skrive til standard error er enkelt. Du kan bruke metoden `STDERR.puts` for å skrive ut en melding til standard error. Her er et eksempel på hvordan du kan gjøre det:

```Ruby
STDERR.puts "Dette er en feilmelding"
```

Dette vil skrive ut teksten "Dette er en feilmelding" til standard error. Her er et annet eksempel som viser hvordan du kan bruke standard error til å håndtere feil i koden din:

```Ruby
begin
  # Kode som kan føre til en feil
rescue StandardError => e
  STDERR.puts "Noe gikk galt: #{e.message}"
end
```

I dette eksempelet bruker vi `begin` og `rescue` for å håndtere en potensiell feil i koden vår. Vi skriver ut feilmeldingen ved hjelp av `STDERR.puts` og får tilbake feilmeldingen ved hjelp av `e.message`.

## Dykk dypere inn i bruken av standard error

Å skrive til standard error er en viktig komponent i feilhåndtering i Ruby. Mange utviklere bruker det også til å logge feil og andre viktige meldinger. Det finnes også forskjellige metoder du kan bruke i tillegg til `puts`, som for eksempel `warn` og `error`, for å gi forskjellige nivåer av viktig informasjon. Det er også mulig å kombinere bruk av standard error med andre feilhåndteringsmetoder som `raise` for å få en mer strukturert og effektiv måte å håndtere feil i koden din.

## Se også
- [Ruby dokumentasjon: Debugging med standard error](https://ruby-doc.org/stdlib-2.7.1/libdoc/logger/rdoc/Logger.html#method-c-new)
- [Stack Overflow: Hva er forskjellen mellom standard error og standard output?](https://stackoverflow.com/questions/3703493/what-is-the-difference-between-stderr-and-stdout-in-ruby)
- [Codecademy: Feilhåndtering i Ruby](https://www.codecademy.com/learn/learn-ruby/modules/exceptions-and-error-handling-in-ruby)
- [Ruby Monstas: Feilhåndtering i Ruby](https://rubymonstas.org/curriculum/intermediate_ruby/error_handling.html)

Husk at å bruke standard error er en viktig del av å skrive robust og feilfri kode i Ruby. Ta deg tid til å lære om det og utforske mulighetene for å bruke det i koden din. Lykke til!