---
title:                "Code organiseren in functies"
date:                  2024-01-28T22:03:14.619305-07:00
model:                 gpt-4-0125-preview
simple_title:         "Code organiseren in functies"

category:             "Ruby"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/ruby/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Je code organiseren in functies splitst je script in herbruikbare delen. Het gaat allemaal om het schoon, beheersbaar en minder foutgevoelig maken van je code. Modulaire code is geweldig omdat het je tijd bespaart, je geestelijke gezondheid behoudt en het debuggen en unit testen vereenvoudigt.

## Hoe te:
Stel je voor dat je een snel script schrijft om gebruikers te begroeten:

```Ruby
def greet(name)
  "Hallo, #{name}!"
end

puts greet("Alice")   # Uitvoer: Hallo, Alice!
puts greet("Bob")     # Uitvoer: Hallo, Bob!
```

Of misschien bereken je de oppervlakte van een cirkel:

```Ruby
def circle_area(radius)
  Math::PI * radius ** 2
end

puts circle_area(5)   # Uitvoer: 78.53981633974483
```

Netter en gemakkelijker te hanteren, toch?

## Diepgaand
Het concept van functies, ook bekend als methoden in Ruby, is niet nieuw - het is zo oud als programmeren zelf. Terugkerend naar de jaren 1950, werden subroutines, zoals ze bekend waren, geïntroduceerd om redundantie te verminderen.

Alternatieven? Zeker, je hebt inline code, je kunt OOP gaan met klassen en objecten, of zelfs functioneel met lambdas en procs. Maar functies zijn de basis van ordelijke code. Wil je prestaties? Lokale variabelen in functies zijn snel en functies kunnen direct waarden retourneren met `return`.

Wat implementatie betreft, je kunt een functie definiëren met `def` en beëindigen met `end`. Je kunt standaardparameters instellen, gebruik maken van splat-operatoren voor variadische functies, en meer. Functies kunnen zo eenvoudig of complex zijn als je hart begeert.

## Zie Ook
- [Ruby's documentatie over methoden](https://ruby-doc.org/core-2.7.0/Method.html)
- [Leer Programmeren door Chris Pine](https://pine.fm/LearnToProgram/)
- [Praktisch Object-Georiënteerd Ontwerp in Ruby door Sandi Metz](https://www.poodr.com/)
