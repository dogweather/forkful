---
title:    "Swift: Konwertowanie ciągu znaków na małe litery"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Każdy programista Swift nieodmiennie spotyka się z koniecznością manipulowania tekstem w swoim kodzie. Czasami wymaga to zmiany wielkości liter, na przykład z małych na duże lub odwrotnie. W tym artykule dowiesz się, dlaczego konwertowanie ciągu znaków na małe litery jest przydatne i jak to zrobić w języku Swift.

## Jak To Zrobić

Konwersja ciągu znaków na małe litery w Swift jest bardzo prosta. Wystarczy użyć metody "lowercased()" na naszym ciągu znaków. Oto przykładowy kod:

```Swift
let string = "HELLO WORLD"
let lowercaseString = string.lowercased()
print(lowercaseString)
```
Output: "hello world"

Proste, prawda? Metoda "lowercased()" zamienia wszystkie litery na małe, niezależnie od ustawień lokalnych urządzenia. Ale co jeśli chcemy zachować tylko pierwszą literę jako dużą? W takim wypadku możemy użyć metody "capitalized()", która zamienia pierwszą literę na dużą, a pozostałe na małe.

Oto przykład:

```Swift
let string = "hElLo wOrLd"
let capitalizedString = string.capitalized()
print(capitalizedString)
```
Output: "Hello World"

Możemy także zastosować metodę "localizedLowercase()", która konwertuje ciąg znaków z uwzględnieniem ustawień lokalnych urządzenia.

## Deep Dive

Podczas konwersji ciągu znaków na małe litery, należy pamiętać o różnych znakach diakrytycznych, które mogą występować w niektórych językach. Metoda "lowercased()" radzi sobie z nimi bez problemu, jednak jeśli chcemy bardziej precyzyjnej konwersji, możemy użyć metody "folding(options: .diacriticInsensitive, locale: nil)". Jest to szczególnie ważne dla naszych użytkowników w Polsce, gdzie często używa się liter z diakrytykami, takimi jak ć, ń czy ż.

Przykład:

```Swift
let string = "PÓŁNOC"
let foldingString = string.folding(options: .diacriticInsensitive, locale: nil)
print(foldingString)
```
Output: "POLNOC"

Dzięki metodom "lowercased()", "capitalized()" i "folding()", możemy swobodnie manipulować ciągami znaków w języku Swift, zgodnie z naszymi potrzebami.

## Zobacz Również

- [Dokumentacja Swift - Manipulowanie tekstem](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [10 najczęściej używanych metod dla ciągów znaków w Swift](https://www.hackingwithswift.com/articles/126/10-rhetorical-tricks-to-write-better-swift-code-literally-character-edition)
- [Dowiedz się więcej o języku Swift](https://www.apple.com/swift/)

*Autor: Jan Kowalski*