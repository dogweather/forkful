---
date: 2024-01-26 04:39:42.119694-07:00
description: "Liczby zespolone to po\u0142\u0105czenie liczb rzeczywistych i urojonych,\
  \ jak `a + bi`, gdzie `i` jest pierwiastkiem kwadratowym z -1. S\u0105 kluczowe\
  \ w dziedzinach\u2026"
lastmod: '2024-03-13T22:44:35.315249-06:00'
model: gpt-4-0125-preview
summary: "Liczby zespolone to po\u0142\u0105czenie liczb rzeczywistych i urojonych,\
  \ jak `a + bi`, gdzie `i` jest pierwiastkiem kwadratowym z -1."
title: Praca z liczbami zespolonymi
weight: 14
---

## Co i dlaczego?
Liczby zespolone to połączenie liczb rzeczywistych i urojonych, jak `a + bi`, gdzie `i` jest pierwiastkiem kwadratowym z -1. Są kluczowe w dziedzinach takich jak inżynieria czy fizyka, aby rozwiązywać problemy, z którymi zwykłe liczby sobie nie radzą.

## Jak to zrobić:
Elm nie ma wbudowanego wsparcia dla liczb zespolonych, więc stworzysz własny typ i funkcje. Oto szybka konfiguracja:

```Elm
type alias Complex =
    { real : Float, imaginary : Float }

add : Complex -> Complex -> Complex
add a b =
    { real = a.real + b.real, imaginary = a.imaginary + b.imaginary }

-- Przykładowe użycie:
a = { real = 3, imaginary = 2 }
b = { real = 1, imaginary = -4 }

sum = add a b
-- suma to { real = 4.0, imaginary = -2.0 }
```

## Dogłębna analiza
Historycznie liczby zespolone nie zawsze były akceptowane. Stały się przełomem w XVI wieku, umożliwiając rozwiązywanie równań sześciennych. Alternatywy w innych językach, jak Python, oferują wbudowane wsparcie dla liczb zespolonych z operacjami "od razu". W Elm wymaga się podejścia „zrób to sam”, jak widzieliśmy. Ale możesz to uczynić tak zaawansowanym, jak potrzebujesz, budując mnożenie, dzielenie i inne operacje, dostosowując kwestie wydajności.

## Zobacz także
- Oficjalna dokumentacja Elm: https://package.elm-lang.org/ do tworzenia niestandardowych typów i opanowania podstaw Elm.
- Miłośnicy historii matematyki mogliby sprawdzić "An Imaginary Tale" autorstwa Paula J. Nahina, aby śledzić podróż liczb zespolonych przez czas.
- Zanurz się w matematycznie zorientowane wyzwania programistyczne na Project Euler (https://projecteuler.net), aby zastosować swoją magię liczb zespolonych.
