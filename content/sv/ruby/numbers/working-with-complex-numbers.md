---
date: 2024-01-26 04:45:15.613906-07:00
description: "Hur man g\xF6r: Ruby g\xF6r hanteringen av komplexa tal mycket enkel.\
  \ Du kan skapa och manipulera dem med hj\xE4lp av klassen Complex."
lastmod: '2024-03-13T22:44:38.422558-06:00'
model: gpt-4-0125-preview
summary: "Ruby g\xF6r hanteringen av komplexa tal mycket enkel."
title: Att arbeta med komplexa tal
weight: 14
---

## Hur man gör:
Ruby gör hanteringen av komplexa tal mycket enkel. Du kan skapa och manipulera dem med hjälp av klassen Complex:

```ruby
require 'complex'

# Skapa komplexa tal
c1 = Complex(3, 4)
c2 = Complex('2+5i')

# Grundläggande operationer
summa = c1 + c2               # => (5.0+9.0i)
differens = c1 - c2           # => (1.0-1.0i)
produkt = c1 * c2             # => (-14.0+23.0i)
kvot = c1 / c2                # => (0.896551724137931+0.03448275862068961i)

# Konjugat, magnitud och fas
konjugat = c1.conjugate       # => (3.0-4.0i)
magnitud = c1.abs             # => 5.0
fas = c1.phase                # Math.atan2(4, 3) => 0.9272952180016122 radianer

# Specifika metoder för komplexa tal
polar = c1.polar              # => [5.0, 0.9272952180016122]
rektangulär = c1.rect         # => [3.0, 4.0]
```

## Fördjupning
Komplexa tal är inte nya - de har funnits sedan 1600-talet, för att lösa ekvationer utan reella lösningar. Bortom matematiken gör sig Rubys Complex-klass av med det tunga lyftet, stöttat av Matematikmodulen för trigonometriska och transcendenta funktioner.

Tidigare programmeringsspråk krävde manuell hantering av reella och imaginära delar. Vissa, som Fortran och C++, har dedikerade bibliotek för komplex aritmetik.

Rubys tillvägagångssätt innebär stöd för komplexa tal i dess syntax, vilket befriar dig från att återuppfinna hjulet. Bakom kulisserna sköter Complex-klassen matematiken, medan Ruby tar hand om objektinteraktioner.

## Se även
- Ruby Dokumentation om Complex: [https://ruby-doc.org/core/Complex.html](https://ruby-doc.org/core/Complex.html)
- MathWorlds syn på komplexa tal: [http://mathworld.wolfram.com/ComplexNumber.html](http://mathworld.wolfram.com/ComplexNumber.html)
- En visuell introduktion till komplexa tal och varför de är användbara: [https://www.youtube.com/watch?v=5PcpBw5Hbwo](https://www.youtube.com/watch?v=5PcpBw5Hbwo)
