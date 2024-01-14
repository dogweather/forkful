---
title:                "Swift: Konwersja tekstu na wielkie litery"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego warto uczyć się kapitalizowania stringów w języku Swift

Język programowania Swift oferuje wiele przydatnych funkcji, w tym możliwość kapitalizowania stringów. Może to być przydatne w wielu sytuacjach, na przykład podczas wyświetlania tekstu użytkownikowi lub tworzenia nazw własnych. W tym artykule dowiesz się, jak wykorzystać tę funkcję i jakie są jej możliwości.

## Jak kapitalizować stringi w języku Swift

Kapitalizowanie stringów można zrobić na kilka sposobów przy użyciu różnych metod dostępnych w języku Swift. Przyjrzyjmy się dwóm najpopularniejszym z nich: `capitalized` i `localizedCapitalized`.

```
Swift
let string = "witaj świecie"
print(string.capitalized)
```

W powyższym przykładzie wywołujemy metodę `capitalized` na zmiennej `string`, która zawiera tekst "witaj świecie". Wynikiem będzie napis "Witaj świecie", gdzie pierwsza litera będzie duża, a pozostałe małe.

Możemy również wykorzystać metodę `localizedCapitalized`, która dostosowuje kapitalizację do języka ustawionego na urządzeniu.

```
Swift
let string = "hello world"
print(string.localizedCapitalized)
```

W powyższym przykładzie wywołujemy metodę `localizedCapitalized` na zmiennej `string`, która zawiera tekst "hello world". Jeśli język ustawiony na urządzeniu to na przykład niemiecki, wynikiem będzie napis "Hello World", ponieważ w tym języku pierwsza litera każdego słowa musi być duża.

## Głębsze spojrzenie na kapitalizowanie stringów w języku Swift

Kapitalizowanie stringów w języku Swift jest bardzo proste, ale funkcja ta może być wykorzystana w bardziej zaawansowany sposób. Na przykład, możemy kapitalizować tylko pierwszą literę całego zdania, a nie każdego słowa.

```
Swift
let string = "to jest przykładowe zdanie do kapitalizacji"
print(string.capitalized(with: .sentences))
```

W tym przypadku wywołujemy metodę `capitalized` z ustawieniem na `.sentences`, co oznacza, że tylko pierwsza litera pierwszego słowa w zdaniu będzie duża. Wynikiem będzie "To jest przykładowe zdanie do kapitalizacji".

Możemy również wykorzystać metodę `capitalized(with: .words)`, która zrobi to samo, ale dla każdego słowa w zdaniu.

```
Swift
let string = "to jest przykładowe zdanie do kapitalizacji"
print(string.capitalized(with: .words))
```

W tym przypadku wynikiem będzie "To Jest Przykładowe Zdanie Do Kapitalizacji".

## Zobacz również

- [Dokumentacja języka Swift](https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language)
- [Tutorial o kapitalizowaniu stringów w języku Swift](https://learnappmaking.com/capitalising-strings-swift-how-to/)
- [Przykłady wykorzystania kapitalizowania stringów w języku Swift](https://www.hackingwithswift.com/example-code/strings/how-to-capitalize-the-first-letter-of-a-string)