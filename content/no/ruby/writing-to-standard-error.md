---
title:                "Ruby: Å skrive til standardfeil"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standard error i Ruby-programmering kan være nyttig for feilsøking av koden din. Det lar deg skrive ut spesielle feilmeldinger som kun vises når det oppstår en feil i programmet, og gir deg detaljert informasjon om hvor feilen skjedde. Dette kan hjelpe deg med å finne og løse feil raskere.

## Hvordan

For å skrive til standard error i Ruby, kan du bruke metoden `STDERR.puts()` med en streng som argument. Dette vil skrive ut strengen til standard error output. Du kan også bruke `STDERR.print()` for å skrive uten en linjeskift på slutten.

```Ruby
x = 10
if x > 5
  STDERR.puts("x er større enn 5")
else
  STDERR.puts("x er mindre enn 5")
end
```

Dette eksemplet vil skrive ut "x er større enn 5" til standard error output, siden 10 er større enn 5. Hvis du ønsker å skrive ut til standard error og standard output samtidig, kan du bruke `puts()` og `STDERR.puts()` sammen.

```Ruby
puts("Dette er en melding som skrives til standard output")
STDERR.puts("Dette er en melding som skrives til standard error")
```

Dette eksemplet vil skrive ut begge meldingene, men "Dette er en melding som skrives til standard error" vil være merket med en rød farge for å indikere at det er en feilmelding.

## Deep Dive

Når du skriver til standard error i Ruby, vil output bli skrevet til standard error stream, som er en del av den standard Ruby-kanalene. Du kan også bruke metoden `Kernel.warn()` for å skrive en advarsel til standard error. Denne metoden viser spesiell fargeformatert output for å tydeliggjøre at det er en advarsel.

```Ruby
x = 0
Kernel.warn("x er lik 0") if x == 0
```

Dette eksemplet vil skrive ut "x er lik 0" med rød fargeformatert output for å indikere en advarsel, siden x er lik 0.

Når du bruker `puts()` eller `print()` for å skrive ut til standard error, vil de bli vist i gult for å skille dem fra standard output. Dette er nyttig for feilsøking og løsning av feil i kode.

## Se Også

- [Ruby Dokumentasjon - Standard Error](https://ruby-doc.org/core-2.7.1/IO.html#method-i-puts)
- [Ruby Dokumentasjon - Kernel.warn()](https://ruby-doc.org/core-2.7.1/Kernel.html#method-i-warn)
- [Om feilsøking i Ruby-programmering](https://www.rubyguides.com/2019/11/debugging-ruby/)