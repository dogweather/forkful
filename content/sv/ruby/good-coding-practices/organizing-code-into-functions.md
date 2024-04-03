---
date: 2024-01-26 01:11:38.584754-07:00
description: "Hur man g\xF6r: T\xE4nk dig att du skriver ett snabbt script f\xF6r\
  \ att h\xE4lsa p\xE5 anv\xE4ndare."
lastmod: '2024-03-13T22:44:38.436415-06:00'
model: gpt-4-1106-preview
summary: "T\xE4nk dig att du skriver ett snabbt script f\xF6r att h\xE4lsa p\xE5 anv\xE4\
  ndare."
title: Att organisera kod i funktioner
weight: 18
---

## Hur man gör:
Tänk dig att du skriver ett snabbt script för att hälsa på användare:

```Ruby
def greet(name)
  "Hej, #{name}!"
end

puts greet("Alice")   # Utmatning: Hej, Alice!
puts greet("Bob")     # Utmatning: Hej, Bob!
```

Eller kanske räknar du ut cirkelns area:

```Ruby
def circle_area(radius)
  Math::PI * radius ** 2
end

puts circle_area(5)   # Utmatning: 78.53981633974483
```

Snyggare och enklare att hantera, eller hur?

## Fördjupning
Konceptet av funktioner, även kända som metoder i Ruby, är inte nytt – det är lika gammalt som programmering i sig. Återgår man till 1950-talet introducerades subrutiner, som de kallades, för att reducera redundans.

Alternativ? Visst, du har inbäddad kod, du kan gå OOP med klasser och objekt, eller till och med funktionellt med lambdas och procs. Men funktioner är grunden för ordnad kod. Vill du ha prestanda? Lokala variabler i funktioner är snabba och funktioner kan returnera värden omedelbart med `return`.

När det gäller implementering kan du definiera en funktion med `def` och avsluta den med `end`. Du kan ställa in standardparametrar, använda splat-operatorer för variadiska funktioner och mer. Funktioner kan vara så enkla eller komplexa som ditt hjärta önskar.

## Se också
- [Rubys metoddokumentation](https://ruby-doc.org/core-2.7.0/Method.html)
- [Learn to Program av Chris Pine](https://pine.fm/LearnToProgram/)
- [Practical Object-Oriented Design in Ruby av Sandi Metz](https://www.poodr.com/)
