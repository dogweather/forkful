---
title:    "Ruby: Leser kommandolinjeargumenter"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Hvorfor

Alle som driver med programmering har sannsynligvis kommet over muligheten til å lese kommandolinjeargumenter. Dette kan være nyttig for å gjøre programmene dine mer fleksible og enklere å bruke. Men hva er egentlig poenget med å lese kommandolinjeargumenter? Les videre for å finne ut hvorfor du bør ha denne kunnskapen i verktøykassen din.

## Hvordan gjøre det

Det er enkelt å lese kommandolinjeargumenter i Ruby. Alt du trenger å gjøre er å bruke ARGV-objektet, som er en innebygd klasse som lar deg få tilgang til argumentene som blir gitt når du kjører programmet ditt. La oss se på et enkelt eksempel:

```Ruby
# program.rb
p ARGV
```

Hvis du kjører dette programmet med følgende kommandolinje:

```
ruby program.rb hello world
```

Vil output være en array med disse argumentene i:

```
["hello", "world"]
```

Som du kan se, er det enkelt å få tilgang til disse argumentene ved hjelp av ARGV. Du kan deretter bruke dem i programmet ditt på en måte som er relevant for det du ønsker å oppnå.

## Dypdykk

Det kan være nyttig å vite at ARGV-objektet også inkluderer det første argumentet som blir gitt når du kjører programmet, som i eksempelet over ville være navnet på programfilen "program.rb". Dette kan være nyttig hvis du trenger å vite navnet på filen som blir kjørt.

Det er også verdt å merke seg at ARGV-objektet inkluderer blant annet argumenter som inneholder spesialtegn eller mellomrom, ved å omslutte dem med dobbelt anførselstegn. For eksempel, hvis du kjører programmet ditt med følgende kommandolinje:

```
ruby program.rb "hello world"
```

Vil output fortsatt være en array med ett element:

```
["hello world"]
```

Dette kan være nyttig å vite når du arbeider med argumenter som inneholder mellomrom eller andre spesialtegn.

## Se også

- Dokumentasjon for ARGV-klassen: https://ruby-doc.org/core-2.7.0/ARGF.html
- En guide til å lese og behandle kommandolinjeargumenter i Ruby: https://www.rubyguides.com/2018/05/ruby-command-line-options/
- En grundig forklaring på hvordan Ruby behandler kommandolinjeargumenter: https://www.digitalocean.com/community/tutorials/how-to-read-command-line-arguments-in-ruby