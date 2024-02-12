---
title:                "Werken met complexe getallen"
date:                  2024-01-28T22:12:40.339553-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met complexe getallen"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/ruby/working-with-complex-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Complexe getallen, bestaande uit een reëel en een imaginair deel (zoals 3+4i), zijn een basiscomponent in engineering en fysica. Programmeurs werken ermee in simulaties, signaalverwerking en bij het oplossen van vergelijkingen die niet goed overweg kunnen met enkel reële getallen.

## Hoe:
Ruby maakt het hanteren van complexe getallen heel eenvoudig. Je kunt ze creëren en manipuleren met behulp van de Complex klasse:

```ruby
require 'complex'

# Creëer complexe getallen
c1 = Complex(3, 4)
c2 = Complex('2+5i')

# Basisbewerkingen
som = c1 + c2               # => (5.0+9.0i)
verschil = c1 - c2          # => (1.0-1.0i)
product = c1 * c2           # => (-14.0+23.0i)
quotiënt = c1 / c2          # => (0.896551724137931+0.03448275862068961i)

# Geconjugeerde, grootte en fase
geconjugeerde = c1.conjugate    # => (3.0-4.0i)
grootte = c1.abs                # => 5.0
fase = c1.phase                 # Math.atan2(4, 3) => 0.9272952180016122 radialen

# Specifieke methoden voor complexe getallen
polair = c1.polar               # => [5.0, 0.9272952180016122]
rechthoekig = c1.rect           # => [3.0, 4.0]
```

## Diepere Duik
Complexe getallen zijn niet nieuw—ze bestaan al sinds de 16e eeuw, voor het oplossen van vergelijkingen zonder reële oplossingen. Naast de wiskunde neemt Ruby’s Complex klasse computertechnisch het zware werk op zich, ondersteund door het Math module voor trigonometrische en transcendente functies.

Eerdere programmeertalen vereisten handmatige behandeling van reële en imaginaire delen. Sommigen, zoals Fortran en C++, wijden speciale bibliotheken aan complexe rekenkunde.

Ruby's benadering omvat complexe getalondersteuning in zijn syntax, waardoor je het wiel niet opnieuw hoeft uit te vinden. Achter de schermen handelt de Complex klasse de wiskunde af, terwijl Ruby zorgt voor objectinteracties.

## Zie Ook
- Ruby Docs over Complex: [https://ruby-doc.org/core/Complex.html](https://ruby-doc.org/core/Complex.html)
- MathWorld's kijk op Complexe Getallen: [http://mathworld.wolfram.com/ComplexNumber.html](http://mathworld.wolfram.com/ComplexNumber.html)
- Een visuele introductie tot complexe getallen en waarom ze nuttig zijn: [https://www.youtube.com/watch?v=5PcpBw5Hbwo](https://www.youtube.com/watch?v=5PcpBw5Hbwo)
