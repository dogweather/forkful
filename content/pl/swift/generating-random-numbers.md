---
title:                "Swift: Generowanie losowych liczb"
programming_language: "Swift"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest nieodłącznym elementem wielu aplikacji i gier. Dzięki nim możliwe jest stworzenie nieskończonych kombinacji i wybór losowych wartości, co przyczynia się do zwiększenia interaktywności i różnorodności projektów programistycznych.

## Jak to zrobić

Generowanie losowych liczb w języku Swift jest bardzo proste dzięki dostępnym wbudowanym funkcjom. Można to zrobić na kilka sposobów, jednym z nich jest użycie funkcji arc4random_uniform(), która zwraca losową wartość całkowitą w podanym zakresie.

```Swift
var randomInt = Int(arc4random_uniform(100)) //wygeneruje liczbę w zakresie od 0 do 99
```

Jeśli chcemy stworzyć losową liczbę zmiennoprzecinkową, możemy użyć funkcji arc4random(), która zwraca wartość od 0 do 0.9999999999999999. W przypadku gdy potrzebujemy bardziej precyzyjnych wartości, możemy również użyć funkcji drand48().

```Swift
var randomDouble = Double(arc4random()) //niesprecyzowany zakres od 0 do 0.9999999999999999
var randomPreciseDouble = drand48() //wyspecyfikowany zakres od 0 do 1
```

Powyższe funkcje mogą być również wykorzystane do generowania losowych wartości w tablicach lub pętlach, co pozwala na stworzenie większej ilości unikalnych kombinacji.

## Deep Dive

Generowanie liczb losowych w języku Swift jest możliwe dzięki użyciu algorytmu generującego pseudolosowe liczby o nazwie Lehmer RNG. W przypadku funkcji arc4random_uniform() wartość ta jest mnożona przez wartość maksymalną zakresu, a następnie zaokrąglona w dół, aby uzyskać liczbę całkowitą. Algorytm ten jest szybki i zapewnia dużą losowość, jednak nie jest zalecany do zastosowań, w których bezpieczeństwo danych jest kluczowe.

## Zobacz również

1. Dokumentacja Swift dotycząca funkcji generujących losowe liczby: https://developer.apple.com/documentation/swift/1540917-random
2. Wprowadzenie do generowania liczb losowych w języku Swift: https://www.hackingwithswift.com/articles/151/how-to-generate-random-numbers-in-swift
3. Tutorial na temat tworzenia gry w języku Swift z wykorzystaniem funkcji random(): https://www.raywenderlich.com/7738344-random-numbers-in-game-development-using-swift