---
title:    "Ruby: Skriver til standard feil"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Hvorfor

Når man skriver programmer, er det viktig å være klar over eventuelle feil som kan oppstå. Dette er derfor hvorfor det er viktig å kunne skrive til standardfeil, eller "standard error" på engelsk, i Ruby-kode.

## Hvordan du gjør det

Det er enkelt å skrive til standardfeil i Ruby ved hjelp av `puts` og `STDERR`. La oss se på et eksempel:

```ruby
def divide(x, y)
  return x / y
rescue ZeroDivisionError => e
  STDERR.puts "Kan ikke dele på null!"
end

puts divide(8, 0) # Dette vil skrive ut "Kan ikke dele på null!" til standardfeil
```

Her definerer vi en funksjon som deler to tall, men hvis det andre tallet er null, vil programmet kaste en `ZeroDivisionError` og skrive ut en feilmelding til standardfeil ved hjelp av `STDERR.puts`.

## Dypdykk

Standardfeil er en av flere "standard streams" i Ruby, sammen med standard inngang (`STDIN`) og standard utgang (`STDOUT`). Standardfeil brukes vanligvis til å skrive ut feilmeldinger og andre viktige meldinger som brukeren må se, men som ikke passer i standard utgang.

En interessant ting å merke seg er at `puts`metoden faktisk skriver til standard utgang (`STDOUT`), men ved å bruke `STDERR.puts`, kan man være sikker på at meldingen vises selv om standard utgang er omdirigert.

## Se også

- [Ruby dokumentasjon om standard streams](https://ruby-doc.org/core-2.7.0/IO.html#class-IO-label-Standard+Streams)
- [Artikkel om standard streams i Ruby](https://www.rubyguides.com/2019/10/ruby-standard-streams/)
- [Eksempler på standard error i Ruby](https://www.geeksforgeeks.org/ruby-error-handling-with-raise-rescue-and-catch/)