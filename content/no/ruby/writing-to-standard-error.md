---
title:    "Ruby: Skriving til standardfeil"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standarderror, også kjent som STDERR, er en viktig del av Ruby programmering. Det lar oss feilsøke og håndtere unntak i våre programmer, noe som kan føre til en bedre og mer pålitelig kode.

## Hvordan å skrive til standarderror

Det er enkelt å skrive til standarderror i Ruby. Alt du trenger å gjøre er å bruke metoden `warn` og plassere meldingen din innenfor parentesene. Her er et eksempel på hvordan du kan bruke `warn` for å skrive en feilmelding til standarderror:

```Ruby
warn("Dette er en feilmelding til standarderror.")
```

Når du kjører denne koden, vil du se følgende output i terminalen:

`Dette er en feilmelding til standarderror.`

Som du kan se, er det enkelt å skrive til standarderror. Du kan også bruke `STDERR.puts` og `STDERR.print` metoder for å skrive til standarderror, men `warn` er den mest vanlige måten å gjøre det på.

## Dykk dypere

Nå som du vet hvordan du skriver til standarderror, la oss se nærmere på hva som faktisk skjer når du gjør det. Når du bruker `warn` metoden, sender Ruby meldingen din til standarderror strømmen. Hvis du ikke har definert en standarderror strøm, vil den bli sendt til standard output strømmen istedenfor.

I tillegg kan du også bruke `rescue` stategi for å håndtere unntak og skrive feilmeldinger til standarderror. Dette kan hjelpe deg med å diagnostisere og feilsøke problemene i koden din.

## Se også

* [Ruby Dokumentasjon om Standarderror](https://ruby-doc.org/core-2.7.1/StandardError.html)
* [Feilsøking i Ruby](https://medium.com/@jbodah/ruby-debugging-101-a66417af196b)
* [Ruby Unntakshåndtering](https://www.rubyguides.com/2019/09/ruby-exception-handling/)