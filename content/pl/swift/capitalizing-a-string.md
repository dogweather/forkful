---
title:    "Swift: Zapisywanie ciągu wielkimi literami"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Kapitalizacja tekstu jest powszechnie stosowaną funkcją w programowaniu, szczególnie podczas pracy z tekstem użytkownika. Jest to sposób na upewnienie się, że tekst będzie wyglądał estetycznie i czytelnie. Może również być wymagane w niektórych przypadkach, np. podczas generowania raportów lub wyświetlania danych w tabeli. W tym artykule dowiecie się, jak prosto zastosować kapitalizację tekstu w języku Swift.

## Jak To Zrobić

Jednym z najprostszych sposobów na kapitalizację tekstu jest użycie wbudowanej metody `capitalized` dla typu `String`. Spójrzmy na przykład poniżej:

```Swift
let word = "programowanie"
print(word.capitalized)
```
Output: "Programowanie"

Jak widać, metoda ta prostym sposobem zmienia pierwszą literę w słowie na wielką, a pozostałe litery na małe.

Możliwe jest również wykorzystanie metody `capitalized` w połączeniu z pętlą for, aby dokonać kapitalizacji dla każdego wyrazu w zdaniu. Przykład poniżej:

```Swift
let sentence = "w tym zdaniu każde słowo będzie zaczynać się wielką literą"
var capitalizedSentence = ""
for word in sentence.components(separatedBy: " ") {
    if !capitalizedSentence.isEmpty {
        capitalizedSentence += " "
    }
    capitalizedSentence += word.capitalized
}
print(capitalizedSentence)
```
Output: "W Tym Zdaniu Każde Słowo Będzie Zaczynać Się Wielką Literą"

## Deep Dive

W języku Swift istnieje również możliwość bardziej zaawansowanej kapitalizacji tekstu, np. z użyciem innych języków lub reguł interpunkcyjnych. Do tego celu możemy wykorzystać metodę `capitalized(with:)`, która przyjmuje jako argument obiekt klasy `Locale`. Przykład użycia:

```Swift
let word = "majątek"
print(word.capitalized(with: Locale(identifier: "pl_PL")))
```
Output: "Majątek"

W tym przypadku, metoda `capitalized` nie wystarczyłaby do prawidłowego wyświetlenia polskiego słowa z dużej litery. Dzięki wykorzystaniu klasy `Locale` i przekazaniu jej identyfikatora języka, możemy zapewnić poprawne kapitalizowanie dla danego języka.

## Zobacz Również

- [Dokumentacja języka Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Tutorial: Manipulowanie tekstem w języku Swift](https://www.raywenderlich.com/166113/manipulating-text-swift-3)
- [Różne metody kapitalizacji tekstu w języku Swift](https://geekytheory.com/como-pasando-texto-a-mayusculas-o-minusculas-en-swift)

Dzięki tym prostym metodom, kapitalizacja tekstu w języku Swift staje się łatwym i przydatnym narzędziem w pracy z tekstem. Mam nadzieję, że ten artykuł pomógł Wam w zrozumieniu tej funkcji. Do zobaczenia w kolejnym wpisie!