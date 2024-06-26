---
date: 2024-01-26 04:45:10.825948-07:00
description: "Hvordan: Ruby gj\xF8r det enkelt \xE5 h\xE5ndtere komplekse tall. Du\
  \ kan opprette og manipulere dem ved \xE5 bruke Complex-klassen."
lastmod: '2024-03-13T22:44:41.309740-06:00'
model: gpt-4-0125-preview
summary: "Ruby gj\xF8r det enkelt \xE5 h\xE5ndtere komplekse tall."
title: "\xC5 jobbe med komplekse tall"
weight: 14
---

## Hvordan:
Ruby gjør det enkelt å håndtere komplekse tall. Du kan opprette og manipulere dem ved å bruke Complex-klassen:

```ruby
require 'complex'

# Opprette komplekse tall
c1 = Complex(3, 4)
c2 = Complex('2+5i')

# Grunnleggende operasjoner
sum = c1 + c2               # => (5.0+9.0i)
differanse = c1 - c2        # => (1.0-1.0i)
produkt = c1 * c2           # => (-14.0+23.0i)
kvotient = c1 / c2          # => (0.896551724137931+0.03448275862068961i)

# Konjugat, størrelse, og fase
konjugat = c1.conjugate    # => (3.0-4.0i)
størrelse = c1.abs          # => 5.0
fase = c1.phase            # Math.atan2(4, 3) => 0.9272952180016122 radianer

# Spesifikke metoder for komplekse tall
polar = c1.polar            # => [5.0, 0.9272952180016122]
rektangulær = c1.rect       # => [3.0, 4.0]
```

## Dypdykk
Komplekse tall er ikke nye - de har vært rundt siden det 16. århundret for å løse ligninger uten reelle løsninger. Bortsett fra matematikk, så tar Ruby's Complex-klasse seg av det tunge løftet beregningsmessig, støttet av Math-modulen for trigonometriske og transcendente funksjoner.

Tidligere programmeringsspråk krevde manuell håndtering av reelle og imaginære deler. Noen, som Fortran og C++, har spesialiserte biblioteker for kompleks aritmetikk.

Rubys tilnærming inkluderer støtte for komplekse tall i sin syntaks, noe som frigjør deg fra å finne opp hjulet på nytt. Bak kulissene tar Complex-klassen seg av matematikken, mens Ruby håndterer objektinteraksjonene.

## Se også
- Ruby Docs om Complex: [https://ruby-doc.org/core/Complex.html](https://ruby-doc.org/core/Complex.html)
- MathWorlds syn på Komplekse tall: [http://mathworld.wolfram.com/ComplexNumber.html](http://mathworld.wolfram.com/ComplexNumber.html)
- En visuell introduksjon til komplekse tall og hvorfor de er nyttige: [https://www.youtube.com/watch?v=5PcpBw5Hbwo](https://www.youtube.com/watch?v=5PcpBw5Hbwo)
