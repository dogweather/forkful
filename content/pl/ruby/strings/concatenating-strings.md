---
date: 2024-01-20 17:35:25.859561-07:00
description: "Jak to zrobi\u0107: Konkatenacja string\xF3w nie zawsze by\u0142a tak\
  \ wydajna jak obecnie. Historycznie, ka\u017Cde u\u017Cycie `+` tworzy\u0142o nowy\
  \ obiekt w pami\u0119ci, co zu\u017Cywa\u0142o\u2026"
lastmod: '2024-04-05T21:53:37.354562-06:00'
model: gpt-4-1106-preview
summary: "Konkatenacja string\xF3w nie zawsze by\u0142a tak wydajna jak obecnie."
title: "\u0141\u0105czenie \u0142a\u0144cuch\xF3w znak\xF3w"
weight: 3
---

## Jak to zrobić:
```Ruby
# Operator plus (+) do łączenia stringów
powitanie = "Cześć, " + "jak " + "się masz?"
puts powitanie
#=> Cześć, jak się masz?

# Interpolacja stringów z użyciem #{}
imie = "Ania"
powitanie = "Hej, #{imie}!"
puts powitanie
#=> Hej, Ania!

# Metoda concat
tekst = "Warszawa"
tekst.concat(" to", " moje", " miasto")
puts tekst
#=> Warszawa to moje miasto

# Operator shovel (<<), nazywany też operatorem łopatki
powitanie = "Witaj"
powitanie << " w " << "świecie Ruby!"
puts powitanie
#=> Witaj w świecie Ruby!
```

## Wgłębiamy się
Konkatenacja stringów nie zawsze była tak wydajna jak obecnie. Historycznie, każde użycie `+` tworzyło nowy obiekt w pamięci, co zużywało zasoby przy dużych lub wielokrotnych konkatenacjach. Ruby 1.9 wprowadziło zmiany w tym zakresie, optymalizując działanie. Alternatywą do `+` jest używanie metod `concat` lub `<<`, które zmieniają obiekt bezpośrednio, nie tworząc niepotrzebnie nowych. W wypadku dużych ilości danych może to znacząco wpłynąć na wydajność. Interpolacja jest zwykle najszybsza i najbardziej polecana, zwłaszcza gdy pracujemy z symbolami oraz zmiennymi.

## Zobacz również
- Ruby Documentation on String: https://ruby-doc.org/core/String.html
- "Effective Ruby" by Peter J. Jones – zawiera rozdział o przetwarzaniu stringów.
- Ruby Style Guide: https://rubystyle.guide/#string-interpolation - omawia konwencje związane z stylami łączenia stringów.
