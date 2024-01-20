---
title:                "Generowanie liczb losowych"
html_title:           "Gleam: Generowanie liczb losowych"
simple_title:         "Generowanie liczb losowych"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Generowanie liczb losowych oznacza tworzenie wartości, które nie mogą być przewidziane logicznie. Programiści robią to, aby dodać nieprzewidywalność i pseudowybór do swoich aplikacji i gier.

## Jak to zrobić:

Podstawowa implementacja w Swift byłaby taka:

```Swift
let randomValue = Int.random(in: 0..<10)
print(randomValue)
```

W wyniku czego może pokazać się dowolna liczba od 0 do 9.

Zaawansowane generowanie liczb losowych z użyciem zaawansowanych funkcji:

```Swift
import GameplayKit

let randomDistribution = GKRandomDistribution(lowestValue: 0, highestValue: 100)
let number = randomDistribution.nextInt()
print(number)
```

Tutaj także możemy otrzymać dowolną liczbę od 0 do 100.

## Głębsze Zrozumienie

Generowanie liczb losowych to starodawna koncepcja, od czasów greckich filozofów do maszyn Enigma używanych podczas II wojny światowej. W języku Swift jest to dość proste z użyciem wbudowanych funkcji, ale takie rozwiązanie ma swoje ograniczenia, na przykład możliwe jest generowanie tylko liczb całkowitych.

Alternatywą jest użycie frameworka GameplayKit, który oferuje więcej możliwości, takich jak dystrybucje nieliniowe, losowość z nasionem i wiele innych.

Podczas wprowadzania dużych liczb losowych w aplikacjach, ważne jest zapewnienie bezpieczeństwa. Swift ma mechanizmy bezpieczeństwa na to, takie jak brak przepełnienia i samoobrona przed błędami.

## Zobacz również

Swift.org - Generowanie liczb losowych: https://docs.swift.org/swift-book/LanguageGuide/Numbers.html

Tutorial Apple Developer dla GameplayKit: https://developer.apple.com/tutorials/swift/wwdc2016/603-introduction-to-gameplaykit

Framework Swift Numerics: https://github.com/apple/swift-numerics