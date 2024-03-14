---
date: 2024-01-27 20:35:26.681079-07:00
description: "Generowanie losowych liczb w programowaniu dotyczy tworzenia warto\u015B\
  ci numerycznych nieterministycznych lub nieprzewidywalnych. Programi\u015Bci u\u017C\
  ywaj\u0105 liczb\u2026"
lastmod: '2024-03-13T22:44:35.750785-06:00'
model: gpt-4-0125-preview
summary: "Generowanie losowych liczb w programowaniu dotyczy tworzenia warto\u015B\
  ci numerycznych nieterministycznych lub nieprzewidywalnych. Programi\u015Bci u\u017C\
  ywaj\u0105 liczb\u2026"
title: Generowanie liczb losowych
---

{{< edit_this_page >}}

## Co i dlaczego?

Generowanie losowych liczb w programowaniu dotyczy tworzenia wartości numerycznych nieterministycznych lub nieprzewidywalnych. Programiści używają liczb losowych z różnych przyczyn, takich jak symulowanie nieprzewidywalności w grach, wybieranie losowych próbek z zestawów danych czy w celach kryptograficznych.

## Jak to zrobić:

Swift oferuje prosty sposób na generowanie losowych liczb za pośrednictwem swojej biblioteki standardowej. Oto, jak to zrobić dla różnych typów numerycznych:

```Swift
// Generowanie losowej liczby całkowitej między 0 a Int.max
let randomInt = Int.random(in: 0...Int.max)
print(randomInt)

// Generowanie losowej liczby zmiennoprzecinkowej między 0.0 a 1.0
let randomDouble = Double.random(in: 0.0...1.0)
print(randomDouble)

// Generowanie losowej wartości Bool
let randomBool = Bool.random()
print(randomBool)
```

Przykładowe wyjście może się różnić, ponieważ przecież mamy do czynienia z losowością. Uruchomienie kodu wiele razy da różne liczby i wartości logiczne.

## Głębsze spojrzenie

Podejście Swifta do generowania liczb losowych opiera się na wydajnym i solidnym generatorze pseudolosowym (PRNG). Przed Swift 4.2, programiści polegali na zewnętrznych bibliotekach lub możliwościach leżących u podstaw platformy, co mogło prowadzić do niespójności w różnych platformach i środowiskach. Z wprowadzeniem rodzimych interfejsów API w Swift 4.2, generowanie liczb losowych stało się zarówno prostsze, jak i bardziej spójne, niezależnie od leżącej u podstawy platformy.

Jednakże, krytyczne jest zrozumienie, że standardowy generator liczb losowych w Swift nie nadaje się do celów kryptograficznych. W przypadku kryptografii, programiści powinni używać ramki `Security` na platformach Apple, która zapewnia dostęp do kryptograficznie bezpiecznych losowych bajtów. Według mojej ostatniej aktualizacji, Swift nie zawiera w swojej bibliotece standardowej generatora losowych liczb kryptograficznych dostępnego na różnych platformach, skłaniając programistów do poszukiwania bibliotek innych firm dla takich potrzeb na platformach innych niż Apple.

W dziedzinie obliczeń naukowych lub sytuacjach wymagających deterministycznego ciągu pseudolosowych liczb (przez co ciąg może być dokładnie odtworzony), generowanie liczb losowych przez Swifta może nie być najlepszym rozwiązaniem bez możliwości ustalenia ziarna generatora. W takich przypadkach, często stosuje się specjalistyczne biblioteki i algorytmy, aby spełnić te precyzyjne wymagania.
