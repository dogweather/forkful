---
date: 2024-01-26 04:41:52.666286-07:00
description: "Liczby zespolone, sk\u0142adaj\u0105ce si\u0119 z cz\u0119\u015Bci rzeczywistej\
  \ i urojonej, s\u0105 niezb\u0119dne w r\xF3\u017Cnych dziedzinach obliczeniowych,\
  \ takich jak in\u017Cynieria, fizyka i\u2026"
lastmod: '2024-03-13T22:44:35.445378-06:00'
model: gpt-4-0125-preview
summary: "Liczby zespolone, sk\u0142adaj\u0105ce si\u0119 z cz\u0119\u015Bci rzeczywistej\
  \ i urojonej, s\u0105 niezb\u0119dne w r\xF3\u017Cnych dziedzinach obliczeniowych,\
  \ takich jak in\u017Cynieria, fizyka i\u2026"
title: Praca z liczbami zespolonymi
---

{{< edit_this_page >}}

## Co i Dlaczego?

Liczby zespolone, składające się z części rzeczywistej i urojonej, są niezbędne w różnych dziedzinach obliczeniowych, takich jak inżynieria, fizyka i przetwarzanie sygnałów. Programiści używają ich do rozwiązywania równań, z którymi liczby rzeczywiste nie mogą sobie poradzić, jak znajdowanie pierwiastków z liczb ujemnych.

## Jak to zrobić:

Haskell obsługuje liczby zespolone za pomocą modułu `Data.Complex`. Oto krótka wycieczka:

```haskell
import Data.Complex

-- Zdefiniuj dwie liczby zespolone
let z1 = 3 :+ 4  -- to jest 3 + 4i
let z2 = 5 :+ (-2)  -- 5 - 2i

-- Operacje arytmetyczne
let sum = z1 + z2  -- 8 :+ 2
let difference = z1 - z2  -- -2 :+ 6
let product = z1 * z2  -- 23 :+ 14
let quotient = z1 / z2  -- 0.20689655172413793 :+ 0.9655172413793104

-- Sprzężenie zespolone
let conjugateZ1 = conjugate z1  -- 3 :+ (-4)

-- Moduł i faza
let magnitudeZ1 = magnitude z1  -- 5.0
let phaseZ1 = phase z1  -- 0.9272952180016122

-- Konwersja z postaci biegunowej do prostokątnej i odwrotnie
let z1Polar = polar z1  -- (5.0,0.9272952180016122)
let fromPolar = mkPolar 5.0 0.9272952180016122  -- tak samo jak z1
```

Przykładowe wyjście po załadowaniu powyższego kodu w GHCi może być:

```haskell
*Main> sum
8.0 :+ 2.0
*Main> product
23.0 :+ 14.0
*Main> magnitudeZ1
5.0
```

## Szczegółowa analiza

Liczby zespolone sięgają XVI wieku, ale zostały powszechnie zaakceptowane znacznie później. Haskell, podobnie jak wiele języków, zapewnia natywną obsługę arytmetyki zespolonej, co ułatwia pracę z tymi liczbami bez implementowania podstawowych matematycznych założeń.

Alternatywą jest budowanie własnego typu liczby zespolonej lub używanie bibliotek dla konkretnych domen, takich jak kwaterniony do grafiki 3D. Ale dla większości przypadków użycia moduł `Data.Complex` w Haskellu jest w zupełności wystarczający.

Wewnątrz, `Data.Complex` to po prostu typ danych łączący dwie wartości `Float` lub `Double`, reprezentujące odpowiednio części rzeczywistą i urojoną. To prosta i efektywna metoda pracy z liczbami zespolonymi na platformie Haskell.

## Zobacz też

Sprawdź te zasoby, aby dowiedzieć się więcej o liczbach zespolonych w Haskellu:

- Oficjalna dokumentacja `Data.Complex` Haskell: [Hackage Data.Complex](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Complex.html)
- Głębsze spojrzenie na typy liczb w Haskellu: [Naucz się Haskella dla swego dobra!](http://learnyouahaskell.com/starting-out#numbers)
- Dla zastosowań, zbadaj algorytmy szybkiej transformacji Fouriera w Haskellu: [Biblioteka FFT Haskell](https://hackage.haskell.org/package/fft)
