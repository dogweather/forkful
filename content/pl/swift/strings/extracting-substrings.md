---
date: 2024-01-20 17:46:55.869688-07:00
description: "(Ekstrakcja podci\u0105g\xF3w - o co chodzi i dlaczego to robimy?) Szukasz\
  \ fragmentu tekstu w szerszym ci\u0105gu znak\xF3w? To ekstrakcja podci\u0105g\xF3\
  w. Programi\u015Bci\u2026"
lastmod: '2024-03-13T22:44:35.743850-06:00'
model: gpt-4-1106-preview
summary: "(Ekstrakcja podci\u0105g\xF3w - o co chodzi i dlaczego to robimy?) Szukasz\
  \ fragmentu tekstu w szerszym ci\u0105gu znak\xF3w? To ekstrakcja podci\u0105g\xF3\
  w. Programi\u015Bci\u2026"
title: "Wycinanie pod\u0142a\u0144cuch\xF3w"
---

{{< edit_this_page >}}

## What & Why?
(Ekstrakcja podciągów - o co chodzi i dlaczego to robimy?)
Szukasz fragmentu tekstu w szerszym ciągu znaków? To ekstrakcja podciągów. Programiści wydzielają podciągi, by manipulować małymi porcjami tekstu bez potrzeby obchodzenia się z całością.

## How to:
(Jak to zrobić: przykłady kodu)
Swift jest jak kuchnia pełna narzędzi. Oto jak wyciągnąć coś smakowitego ze Stringa:

```Swift
let wholeString = "Witaj, programisto!"
let indexStartOfText = wholeString.index(wholeString.startIndex, offsetBy: 7)
let indexEndOfText = wholeString.index(wholeString.startIndex, offsetBy: 19)

// Wyciąganie podciągu
let substring = wholeString[indexStartOfText..<indexEndOfText] // "programisto"

// Konwersja podciągu na String
let newString = String(substring)

print(newString) // Wypisze "programisto"
```

Proste, prawda?

## Deep Dive:
(Bliskie spojrzenie: głębsze informacje)
Kiedyś, w Swift 3, łatwiej było wyciągnąć podciągi, ale też łatwiej było stracić wydajność. W Swift 4 pojawili się `Substring` i `Range`, aby zadbać o wydajność i bezpieczeństwo typów. Jeżeli chodzi o alternatywy, to mamy `NSString` z Objective-C, ale to już raczej relikty. Swift jest zbudowany w taki sposób, że manipulacja Stringami ma być jak najprostsza z możliwych. Jednakże, zawsze pamiętaj o zarządzaniu pamięcią – tworzenie nowych Stringów z podciągów może być kosztowne!

## See Also:
(Zobacz także)
- [Swift Documentation: String and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift Standard Library: Substring](https://developer.apple.com/documentation/swift/substring)
