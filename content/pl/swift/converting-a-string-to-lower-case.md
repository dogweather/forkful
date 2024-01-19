---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "Fish Shell: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Konwersja łańcucha na małe litery oznacza przekształcenie wszystkich liter w łańcuchu na małe litery. Programiści robią to, aby ułatwić porównywanie i manipulację łańcuchami, ignorując różnice między literami dużymi i małymi.

## Jak to zrobić:

W Swift, chcąc przekształcić łańcuch na małe litery, używamy metody `.lowercased()`. 

```Swift
let originalString = "Hello, World!"
let lowercasedString = originalString.lowercased()

print(lowercasedString)  // Wyprintuje: "hello, world!"
```

Metoda ta zwraca kopię oryginalnego łańcucha, w której wszystkie litery są małe.

## Głębsze zrozumienie

Konwersja do małych liter jest powszechną praktyką w szeregach języków programowania, mającą swoje korzenie w najwcześniejszych dniach komputerów, kiedy to różnice między literami dużymi i małymi były często ignorowane.

Alternatywą dla `.lowercased()` jest metoda `.uppercased()`, która przekształca łańcuch na duże litery, ułatwiając porównywanie łańcuchów pod kątem różnic wielkości liter.

Warto dodać, że `.lowercased()` w Swift jest metodą instancji `String`, co oznacza, że działa na konkretnej instancji typu `String`. Wykorzystuje unicodowe algorytmy, co czyni ją bardzo skuteczną w radzeniu sobie z szerokim zakresem znaków.

## Zobacz też

Bardziej szczegółowe informacje odnośnie manipulacji łańcuchami w Swift można znaleźć w oficjalnej dokumentacji Swift w sekcji o typach łańcuchów i znaków: [String and Character Types in Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html).

Jeżeli jesteś zainteresowany usprawnieniem manipulacji łańcuchami w swoim kodzie, przyjrzyj się również bibliotece [Swift StringCheese](https://github.com/Weebly/StringCheese), która oferuje wiele dodatkowych funkcji do pracy z łańcuchami.