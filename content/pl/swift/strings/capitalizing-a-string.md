---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:33.346792-07:00
description: "Zmiana pierwszej litery ci\u0105gu na wielk\u0105 w Swift polega na\
  \ modyfikacji danego ci\u0105gu znak\xF3w, tak aby jego pierwsza litera by\u0142\
  a wielka, a pozosta\u0142e ma\u0142e.\u2026"
lastmod: '2024-03-13T22:44:35.738117-06:00'
model: gpt-4-0125-preview
summary: "Zmiana pierwszej litery ci\u0105gu na wielk\u0105 w Swift polega na modyfikacji\
  \ danego ci\u0105gu znak\xF3w, tak aby jego pierwsza litera by\u0142a wielka, a\
  \ pozosta\u0142e ma\u0142e.\u2026"
title: "Zamiana liter na wielkie w \u0142a\u0144cuchu znak\xF3w"
weight: 2
---

## Co i dlaczego?

Zmiana pierwszej litery ciągu na wielką w Swift polega na modyfikacji danego ciągu znaków, tak aby jego pierwsza litera była wielka, a pozostałe małe. Programiści robią to w celach takich jak formatowanie imion lub zdań zgodnie z zasadami gramatyki lub standardami interfejsu użytkownika.

## Jak to zrobić:

Struktury `String` w Swifcie zawierają kilka wbudowanych metod do manipulowania wielkością liter w ciągach. Oto kilka sposobów na zmianę ciągów na wielkie litery w Swift, w tym użycie standardowych metod i bibliotek stron trzecich, jeśli jest to konieczne.

### Korzystanie z wbudowanych metod

Aby zamienić pierwszą literę ciągu na wielką, a resztę na małe:

```swift
let myString = "hello, world"
let capitalizedString = myString.prefix(1).uppercased() + myString.dropFirst().lowercased()
print(capitalizedString) // Wynik: "Hello, world"
```

Aby zamienić pierwszą literę każdego słowa w zdaniu, można użyć właściwości `capitalized`:

```swift
let sentence = "hello, world"
let capitalizedSentence = sentence.capitalized
print(capitalizedSentence) // Wynik: "Hello, World"
```

### Korzystanie z biblioteki strony trzeciej

Chociaż standardowa biblioteka Swifta jest dość obszerna, niektóre specyficzne formaty kapitalizacji mogą wymagać bardziej skomplikowanych operacji lub mogą być uproszczone za pomocą bibliotek stron trzecich. Jedną z popularnych do manipulacji ciągami znaków jest SwiftRichString. (Uwaga: Zawsze upewnij się, że dołączasz biblioteki stron trzecich za pomocą Swift Package Manager, CocoaPods lub Carthage i importujesz je do swojego pliku.)

Najpierw musisz dodać `SwiftRichString` do swojego projektu. Po zainstalowaniu możesz go użyć do wykonania różnych operacji na ciągach znaków, w tym specyficznych potrzeb kapitalizacji. Jednakże, jak dotąd, wbudowane metody Swifta adekwatnie pokrywają większość przypadków użycia kapitalizacji bez konieczności używania zewnętrznych bibliotek tylko dla zmiany wielkości liter w ciągach.

Zawsze odwołuj się do najnowszej dokumentacji biblioteki w poszukiwaniu jakichkolwiek aktualizacji lub zmian w metodach.
