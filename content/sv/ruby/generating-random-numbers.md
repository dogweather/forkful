---
title:                "Generera slumpmässiga tal"
date:                  2024-01-20T17:49:46.324455-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generera slumpmässiga tal"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att generera slumptal är en process där man skapar ett tal som inte kan förutsägas bättre än med ren chans. Programmerare använder det för spel, simuleringar, tester och där överraskningselement eller säkerhet är viktigt.

## Hur gör man:
I Ruby använder vi `rand` och `Random` klassen för att generera slumptal. Här är några exempel:

```Ruby
# Enkelt slumptal mellan 0 och 1
puts rand

# Slumptal mellan 0 och 10
puts rand(11)

# Slumptal mellan 1 och 10
puts rand(1..10)

# Skapa ett nytt Random-objekt och generera ett slumptal
my_random = Random.new
puts my_random.rand(100)

# Utskrifter kan variera eftersom de är slumpmässiga!
```

## Fördjupning
Tidigt i programmeringens historia användes enkla algoritmer för att skapa slumptal, men dessa var ofta förutsägbara. Nu är pseudoslumptalsgeneratorer (PRNG) standard, som i Ruby’s `Random` klass, vilket är baserat på en mer avancerad algoritm.

Det finns flera alternativ i Ruby för olika ändamål:
- `SecureRandom` för kryptografiska ändamål.
- `rand` för enklare slumptalsgenerering.

Implementationsdetaljerna i `Random` klassen bygger på Mersenne Twister algoritmen, som ger ett långt mönster innan det upprepas (upp till 2**19937-1).

## Se också
- Ruby’s officiella dokumentation för klassen `Random`: [Ruby-Doc.org Random](https://ruby-doc.org/core/Random.html)
- En introduktion till pseudoslumptalsgeneratorer: [Wikipedia Pseudorandom Number Generator](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)