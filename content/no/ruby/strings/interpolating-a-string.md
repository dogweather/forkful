---
date: 2024-01-20 17:51:42.594301-07:00
description: "Interpolering av strenger i Ruby lar deg sette variabler eller Ruby-kode\
  \ inn i en streng. Vi gj\xF8r det for \xE5 bygge dynamiske strenger raskt og rent."
lastmod: '2024-02-25T18:49:39.480026-07:00'
model: gpt-4-1106-preview
summary: "Interpolering av strenger i Ruby lar deg sette variabler eller Ruby-kode\
  \ inn i en streng. Vi gj\xF8r det for \xE5 bygge dynamiske strenger raskt og rent."
title: Interpolering av en streng
---

{{< edit_this_page >}}

## What & Why?
Interpolering av strenger i Ruby lar deg sette variabler eller Ruby-kode inn i en streng. Vi gjør det for å bygge dynamiske strenger raskt og rent.

## How to:
Interpolering gjøres med `#{}` inni en dobbeltfnuttet streng. Her er et eksempel:

```Ruby
name = "Ola"
greeting = "Hei, #{name}!"
puts greeting
```

Output vil være:
```
Hei, Ola!
```
Du kan også interpolere uttrykk:
```Ruby
price = 250
message = "Totalprisen er #{price * 1.25} kroner, inkludert mva."
puts message
```

Output:
```
Totalprisen er 312.5 kroner, inkludert mva.
```

## Deep Dive
Interpolering i Ruby er ikke bare nyttig, det er også effektivt. I motsetning til konkatenasjon, som binder sammen strenger, skriver Ruby minnevennlig kode ved å vurdere uttrykket inni `#{}` direkte.

Historisk sett har Ruby alltid fremmet en "There's more than one way to do it" filosofi, noe som betyr at du har alternativer som `+` eller `concat` for å bygge opp strenger, men disse kan bli klønete med flere verdier.

Ruby evaluere uttrykk inne i interpoleringen, så pass på å ikke sette inn kode som har bivirkninger. For eksempel, `#{puts 'hei'}` vil skrive ut 'hei' når strengen evalueres.

## See Also
Ta en titt på disse for å dykke dypere:
- Ruby's offisielle dokumentasjon om [String Interpolation](https://ruby-doc.org/core-3.1.0/String.html#label-Interpolation)
- [Ruby Style Guide](https://rubystyle.guide/#string-interpolation) som promoterer god kodestil for interpolering.
