---
title:    "Swift: Zapisywanie ciągu znaków"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Dlaczego

Cześć programistów! Dzisiaj przyjrzymy się funkcji, która może wydawać się jedną z tych prostych i niepozornych, a jednak jest niezwykle przydatna w naszych projektach - chodzi o funkcję pochodzącą z biblioteki Foundation, która umożliwia nam kapitalizowanie stringów. Ale czemu w ogóle chcielibyśmy capitalizować stringi i jak to zrobić?

## Jak to zrobić

Kapitalizacja stringa odnosi się do zmiany pierwszej litery w danym stringu na wielką literę. Jest to przydatne w wielu sytuacjach, na przykład gdy chcemy poprawić estetykę wyświetlanych danych lub gdy musimy dopasować stringi do określonych wymagań. Aby tego dokonać, wystarczy wywołać metodę "capitalized" na danym stringu. Spróbujmy:

```Swift
let greeting = "cześć"
print(greeting.capitalized)
```

To wydrukuje "Cześć" na konsoli. Proste, prawda? Ale co jeśli chcemy kapitalizować więcej niż jedno słowo?

```Swift
let phrase = "witaj w Swiftcie"
print(phrase.capitalized)
```

W tym przypadku metoda "capitalized" kapitalizuje każde słowo w stringu, więc wynik będzie wyglądał tak: "Witaj W Swiftcie". Ale co w przypadku stringów, które zawierają już wyrazy z dużymi literami - czy nie będzie problemu z kapitalizacją? Nie martw się, funkcja "capitalized" jest inteligentna i nie zmieni wyrazów, które są zapisane z dużymi literami. Sprawdź:

```Swift
let sentence = "lUBIĘ pOGRAMOWAĆ w sWIftcie"
print(sentence.capitalized)
```

Wynik? "Lubię Programować W Swiftcie". I znów, proste i szybkie!

## Sekretne mechanizmy

Warto pamiętać, że funkcja "capitalized" nie tylko zmienia pierwszą literę na dużą, ale także zamienia resztę stringa na małe litery. Ale co jeśli chcemy zachować pierwotny stan litery w innych miejscach w stringu? Wtedy możemy użyć metody "capitalizingFirstLetter" lub "capitalizingWords". Pierwsza z nich zachowa wielką literę na początku stringa, a druga będzie zachowywać wielką literę na początku każdego słowa, nawet jeśli już była zapisana małą literą. Przetestujmy to w praktyce:

```Swift
let example = "swIft jeST Fajny!"
print(example.capitalizingFirstLetter)
print(example.capitalizingWords)
```

Output: "SwIft jest fajny!" i "Swift Jest Fajny!".

I jeszcze jedna ważna uwaga - wszystkie te metody zwracają nowy string, więc musimy przypisać ich wynik do nowej stałej lub zmiennej. Jeśli chcielibyśmy zmienić oryginalny string, musimy użyć metody "capitalize" z dużej litery.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o tym, jak manipulować stringami w języku Swift, polecamy następujące źródła: 

1. [Oficjalna dokumentacja biblioteki Foundation](https://developer.apple.com/documentation/foundation/nsstring/1414087-capitalized)
2. [Artykuł na temat String Operations w Swift](https://www.hackingwithswift.com/articles/181/how-to-capitalize-the-first-letter-of-a-string)
3. [Podstawowe operacje na stringach](https://medium.com/ios-os-x-development/basic-string-operations-in-swift-string-regular-expression-3a5fd262ee27)
4. [Krótki film z tutorialami na temat stringów w Swift](https://learnappmaking.com/using-swifts-strings-how-to/)

Dziękujemy za przeczytanie nas