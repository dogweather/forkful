---
title:                "Interpolacja ciągu znaków"
html_title:           "C++: Interpolacja ciągu znaków"
simple_title:         "Interpolacja ciągu znaków"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Interpolacja stringów to inaczej sposób formatowania, który umożliwia wstawianie zmiennych lub wyrażeń bezpośrednio do stringów. Programiści używają jej, ponieważ zwiększa to czytelność i efektywność kodu.

## Jak to zrobić:
```Swift
let imie = "Jan"
let powitanie = "Cześć, \(imie)!"
print(powitanie)
```
Wyjście: `Cześć, Jan!`

Możemy również dodać wyrażenia:
```Swift
let godzina = 10
let powitanie = "Jest \(godzina), czas wstać, \(imie)!"
print(powitanie)
```
Wyjście: `Jest 10, czas wstać, Jan!`
## Głębiej
(1) W kontekście historycznym, interpolacja jest stara jak sama informatyka. Prawie każdy nowoczesny język programowania oferuje tę funkcjonalność. 

(2) Alternatywą dla interpolacji stringów jest łączenie stringów przy użyciu operatorów lub funkcji, ale to nie jest tak wygodne lub czytelne. 

(3) Interpolacja stringów w Swift wykonuje się w czasie wykonywania kodu, co oznacza, że interpolacja dynamiczna jest możliwa i nie musisz znać wszystkich wartości z góry.

## Zobacz także
1. [Dokumentacja Swift na temat String Interpolation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
2. [Tutorial na temat interpolacji stringów w Swift](https://www.hackingwithswift.com/articles/178/super-powered-string-interpolation-in-swift-5-0)
3. [Źródło Swift na GitHub](https://github.com/apple/swift)
4. [Dyskusje o Swift na StackOverflow](https://stackoverflow.com/questions/tagged/swift)