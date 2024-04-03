---
date: 2024-01-26 03:44:27.690127-07:00
description: "Zaokr\u0105glanie liczb polega na modyfikacji liczby dziesi\u0119tnej\
  \ do jej najbli\u017Cszej warto\u015Bci ca\u0142kowitej lub do okre\u015Blonej liczby\
  \ miejsc po przecinku.\u2026"
lastmod: '2024-03-13T22:44:35.316166-06:00'
model: gpt-4-0125-preview
summary: "Zaokr\u0105glanie liczb polega na modyfikacji liczby dziesi\u0119tnej do\
  \ jej najbli\u017Cszej warto\u015Bci ca\u0142kowitej lub do okre\u015Blonej liczby\
  \ miejsc po przecinku."
title: "Zaokr\u0105glanie liczb"
weight: 13
---

## Jak to zrobić:
Moduł `Basics` w Elm dostarcza sprytne funkcje do zaokrąglania: `round`, `floor` i `ceiling`. Oto jak ich używać.

```elm
import Basics exposing (round, floor, ceiling)

-- Zaokrąglenie do najbliższej liczby całkowitej
round 3.14    --> 3
round 3.5     --> 4

-- Zaokrąglenie w dół
floor 3.999   --> 3

-- Zaokrąglenie w górę
ceiling 3.001 --> 4

-- Obcięcie miejsc dziesiętnych bez zaokrąglania
truncate 3.76 --> 3
```

Elm dostarcza również `toLocaleString` do zaokrąglania do stałej liczby miejsc dziesiętnych:

```elm
import Float exposing (toLocaleString)

-- Zaokrąglenie do dwóch miejsc po przecinku
toLocaleString 2 3.14159 --> "3.14"
```

## Wnikliwe spojrzenie
Elm to silnie typowany język funkcyjny, który releguje efekty uboczne do "krawędzi" architektury. Oznacza to, że funkcje takie jak zaokrąglanie muszą być czyste i przewidywalne. Historycznie, zaokrąglanie jest powszechną operacją w wielu językach programowania, które radzą sobie z nieprecyzyjnością arytmetyki liczby zmiennoprzecinkowej.

Podejście Elm do zaokrąglania jest proste - funkcje są czyste i zgodne z matematycznymi definicjami zaokrąglania, podłogi i sufitu. Elm przewiduje wspólne potrzeby, dostarczając wbudowane funkcje, ponieważ zarządzanie precyzją jest częstym wymaganiem, zwłaszcza w finansach i grafice.

Alternatywy dla wbudowanych funkcji Elm mogą obejmować niestandardowe implementacje przy użyciu operacji arytmetycznych, ale dodaje to niepotrzebną złożoność, gdy biblioteka standardowa już wykonuje pracę efektywnie.

W obecnej wersji Elm używa podstawowej arytmetyki zmiennoprzecinkowej JavaScript, utrzymując zgodność ze standardem IEEE 754, co jest ważne do zapamiętania podczas rozważania precyzji i potencjalnych błędów zmiennoprzecinkowych.

## Zobacz również
- Oficjalna dokumentacja modułu `Basics` Elm: https://package.elm-lang.org/packages/elm/core/latest/Basics
- Szczegółowe spojrzenie na działanie liczb zmiennoprzecinkowych w informatyce: https://floating-point-gui.de/
- Moduł `Float` Elm do dalszych operacji na liczbach zmiennoprzecinkowych: https://package.elm-lang.org/packages/elm/core/latest/Float
