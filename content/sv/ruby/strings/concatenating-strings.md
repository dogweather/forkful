---
date: 2024-01-20 17:35:32.461820-07:00
description: "Hur man g\xF6r: ."
lastmod: '2024-03-13T22:44:38.420584-06:00'
model: gpt-4-1106-preview
summary: .
title: "Sammanslagning av str\xE4ngar"
weight: 3
---

## Hur man gör:
```Ruby
# Exempel på konkatenering med '+'
halsning = "Hej " + "världen!"
puts halsning # => Hej världen!

# Använda '<<' för att lägga till en sträng till en annan
namn = "Karl"
namn << "son"
puts namn # => Karlson

# Interpolation med #{}
anvandarnamn = "Anna"
meddelande = "Välkommen, #{anvandarnamn}!"
puts meddelande # => Välkommen, Anna!
```

## Fördjupning
Konkatenering av strängar har alltid varit en grundläggande del av programmering eftersom det tillåter för sammanslagning av information. Förr användes ofta operatören '+', men Ruby införde interpolation och '<<' som är effektivare. Interpolation hanterar automatiskt to_s för objekt och '<<' modifierar strängen på plats vilket sparar minne. Alternativ till konkatenering inkluderar strängbyggnad med 'join' för arrayer och 'concat'-metoden för strängobjekt.

## Se även
- Ruby dokumentation för strängar: [String - Ruby-Doc.org](https://ruby-doc.org/core-3.1.0/String.html)
- Artikel om Ruby stränginterpolation: [Ruby String Interpolation - ThoughtCo](https://www.thoughtco.com/string-interpolation-in-ruby-2908199)
- Ruby style guide för strängar: [Ruby Style Guide - GitHub](https://github.com/rubocop/ruby-style-guide#strings)
