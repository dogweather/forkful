---
title:                "Łączenie ciągów znaków"
html_title:           "Swift: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego to robimy?

Kiedy piszesz kod, może się zdarzyć, że chcesz połączyć wiele ciągów znaków w jeden. W języku Swift, nazywa się to "konkatenacją" i jest to powszechna praktyka w programowaniu. Używając konkatenacji, możesz łatwo i szybko łączyć różne elementy w jeden łańcuch tekstowy.

## Jak to zrobić:

```Swift
let imie = "Anna"
let nazwisko = "Kowalska"
let pelneImie = imie + " " + nazwisko
print(pelneImie) // wyświetli "Anna Kowalska"
```

Możesz również użyć metody `append` aby dodać ciąg znaków do już istniejącego łańcucha:

```Swift
var opis = "Mam "
let wiek = 25
opis.append(String(wiek))
print(opis) // wyświetli "Mam 25"
```

## Zagłębienie:

W językach programowania takich jak C i Java, nie ma wbudowanej funkcjonalności konkatenacji i programiści muszą tworzyć specjalne funkcje, aby osiągnąć ten sam efekt. W Swift, konkatenacja jest wbudowana i może być używana bez dodatkowego kodu. Możesz również użyć operatora `+=` jako skróconej wersji metody `append`.

Inną opcją jest użycie struktury `Joiner`, która jest dostępna od Swift 4.2. Pozwala ona na bardziej wydajne łączenie wielu ciągów znaków, szczególnie w przypadku dużej ilości danych.

Na koniec, warto zwrócić uwagę, że konkatenacja nie jest efektywna w przypadku dużej ilości danych i może powodować problemy wydajnościowe. W takich przypadkach lepiej użyć struktury `StringBuilder` lub `StringBuffer` w językach takich jak Java lub C#, która pozwala na dodawanie danych w sposób bardziej wydajny.

## Zobacz również:

- [Dokumentacja Swift: Konkatenacja i mutowanie łańcuchów znaków](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID284)
- [Porównanie wydajności konkatenacji w różnych językach programowania](https://softwareengineering.stackexchange.com/questions/117848/why-is-string-concatenation-overhead-big)
- [Dokumentacja Swift: Struktura Joiner](https://developer.apple.com/documentation/swift/joiner)