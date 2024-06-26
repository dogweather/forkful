---
date: 2024-01-26 01:11:46.688282-07:00
description: "Hvordan: Tenk deg at du skriver et kjapt script for \xE5 hilse p\xE5\
  \ brukere."
lastmod: '2024-03-13T22:44:41.333844-06:00'
model: gpt-4-1106-preview
summary: "Tenk deg at du skriver et kjapt script for \xE5 hilse p\xE5 brukere."
title: Organisering av kode i funksjoner
weight: 18
---

## Hvordan:
Tenk deg at du skriver et kjapt script for å hilse på brukere:

```Ruby
def hilse(navn)
  "Hallo, #{navn}!"
end

puts hilse("Alice")   # Utdata: Hallo, Alice!
puts hilse("Bob")     # Utdata: Hallo, Bob!
```

Eller kanskje regner du ut arealet av en sirkel:

```Ruby
def sirkel_areal(radius)
  Math::PI * radius ** 2
end

puts sirkel_areal(5)   # Utdata: 78.53981633974483
```

Ryddigere og lettere å håndtere, ikke sant?

## Dypdykk
Konseptet med funksjoner, også kjent som metoder i Ruby, er ikke nytt – det er like gammelt som programmering selv. Tilbake til 1950-tallet, ble underprogrammer, som de ble kalt, introdusert for å redusere redundans.

Alternativer? Selvfølgelig, du har in-line kode, du kan gå OOP (Objektorientert Programmering) med klasser og objekter, eller til og med funksjonell med lambdas og procs. Men funksjoner er grunnlaget for ordentlig kode. Ønsker du ytelse? Lokale variabler i funksjoner er raske og funksjoner kan returnere verdier umiddelbart med `return`.

Når det gjelder implementering, kan du definere en funksjon med `def` og avslutte den med `end`. Du kan sette standardparametere, bruke splat-operatører for variadiske funksjoner, og mer. Funksjoner kan være så enkle eller komplekse som du ønsker.

## Se Også
- [Ruby sin metodedokumentasjon](https://ruby-doc.org/core-2.7.0/Method.html)
- [Learn to Program av Chris Pine](https://pine.fm/LearnToProgram/)
- [Practical Object-Oriented Design in Ruby av Sandi Metz](https://www.poodr.com/)
