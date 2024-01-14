---
title:    "Swift: Generowanie losowych liczb"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# Dlaczego generowanie liczb losowych jest ważne w programowaniu?

Generowanie liczb losowych jest niezbędnym elementem wielu aplikacji. Może być wykorzystane do symulacji różnych przypadkowych zdarzeń, np. w grach wideo, losowania nagród lub generowania unikalnych identyfikatorów. Ponadto, może również służyć jako narzędzie do testowania kodu, gdy potrzebujemy wprowadzić elementy losowości.

# Jak to zrobić w języku Swift?

Generowanie liczb losowych w języku Swift jest bardzo proste i można to zrobić na kilka sposobów. Najprostszym sposobem jest użycie funkcji `arc4random ()` do wygenerowania losowej liczby całkowitej w zakresie od 0 do INT_MAX. 

```Swift
let randomInt = Int(arc4random())
print(randomInt) // wyświetli losową liczbę całkowitą
```

Możemy również określić konkretny zakres używając funkcji `arc4random_uniform(_:)`, która przyjmuje jeden argument określający górny limit liczb. Na przykład, aby wygenerować liczbę losową od 1 do 10, użylibyśmy funkcji w ten sposób: 

```Swift
let randomNum = Int(arc4random_uniform(10)) + 1
print(randomNum) // wyświetli losową liczbę od 1 do 10
```

Możemy również wykorzystać funkcję `arc4random_uniform(_:)` do generowania liczb losowych zmiennoprzecinkowych w określonym zakresie. Aby to zrobić, używamy funkcji `Float()` lub `Double()` do przekonwertowania wygenerowanej liczby na odpowiedni typ:

```Swift
let randomFloat = Float(arc4random_uniform(50)) + 10
print(randomFloat) // wyświetli losową liczbę zmiennoprzecinkową od 10 do 50
```

# Deep Dive

Funkcje `arc4random()` i `arc4random_uniform(_:)` są oparte na generatorze liczb pseudolosowych, który jest częścią biblioteki C w języku Swift. Ten generator używa algorytmu znanego jako XORShift, który jest szybszy i bardziej efektywny niż inne metody generowania liczb losowych.

Pamiętaj jednak, że język Swift oferuje również wbudowane typy, takie jak `Int.random(in:)` czy `Float.random(in:)`, które pozwalają na bardziej kontrolowane generowanie liczb losowych w określonym zakresie. Jednak jeśli potrzebujemy jedynie szybkiego sposobu na wygenerowanie liczb losowych, powinniśmy wybrać funkcje `arc4random()` lub `arc4random_uniform(_:)`.

# Zobacz również

- [Dokumentacja języka Swift](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html)
- [Poradnik generowania liczb losowych w Swift](https://learnappmaking.com/random-numbers-swift/)
- [Przydatne funkcje związane z generowaniem liczb losowych w Swift](https://www.hackingwithswift.com/articles/57/how-to-generate-random-numbers-using-random-and-gkarc4random)