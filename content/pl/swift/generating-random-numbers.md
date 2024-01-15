---
title:                "Generowanie losowych liczb"
html_title:           "Swift: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest bardzo przydatną umiejętnością w programowaniu, ponieważ pozwala ona na wykonywanie różnych zadań, takich jak symulowanie losowych zdarzeń lub tworzenie losowych danych do testowania aplikacji.

## Jak to zrobić

Aby wygenerować losową liczbę w Swift, możemy skorzystać z funkcji `arc4random_uniform(_:UInt32)`. Ta funkcja zwraca losową liczbę całkowitą z przedziału 0 do podanej liczby całkowitej. Przykładowy kod wyglądałby następująco:

```Swift
let randomNumber = Int(arc4random_uniform(10))
print(randomNumber)
```

Ten kod wygeneruje losową liczbę całkowitą od 0 do 9 i wyświetli ją w konsoli. Możemy również wykorzystać tę funkcję do generowania losowych liczb z zakresu innych typów danych, takich jak Double lub Float.

## Głębszy wgląd

W Swift istnieje również struktura `Random`, która umożliwia nam jeszcze większą kontrolę nad generowaniem losowych liczb. Dzięki niej możemy definiować własne zakresy i typy danych dla wygenerowanych liczb. Przykładowy kod wykorzystujący strukturę `Random` mógłby wyglądać tak:

```Swift
let generator = Random()
let randomNumber = generator.generate(normalIn: 1.0...10.0)
print(randomNumber)
```

W tym przypadku wygenerowana liczba będzie typu Double, znajdująca się w zakresie od 1.0 do 10.0.

## Zobacz również

- [Dokumentacja Swift: Generowanie Liczb Losowych](https://developer.apple.com/documentation/swift/int/2952598-arc4random_uniform)
- [Szybkie losowanie liczb losowych w Swift](https://www.hackingwithswift.com/example-code/system/how-to-generate-random-numbers-in-swift)
- [Tworzenie własnej struktury Random w Swift](https://appventure.me/2015/08/24/creating-a-custom-swift-random-number-generator/)