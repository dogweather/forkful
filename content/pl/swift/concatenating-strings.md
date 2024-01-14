---
title:                "Swift: Łączenie ciągów znaków."
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Ciągłe operacje na łańcuchach znaków są nieodłączną częścią programowania w języku Swift. Wiele z nas korzysta z funkcji concat (konkatenacji) do połączenia dwóch lub więcej łańcuchów. Ale dlaczego właściwie powinniśmy używać konkatenacji i jak to zrobić w praktyce? W tym artykule dowiesz się dlaczego stosowanie konkatenacji jest ważne i jak wykorzystać ją w swoich projektach.

## Jak to zrobić

Do konkatenacji używamy operatora "+" lub metody "append()". Przykładowo, chcąc połączyć imię i nazwisko, możemy napisać:
```Swift
let firstName = "Jan"
let lastName = "Kowalski"

let fullName = firstName + " " + lastName
print(fullName)
// Output: Jan Kowalski

var fullName2 = ""
fullName2.append(firstName)
fullName2.append(" ")
fullName2.append(lastName)
print(fullName2)
// Output: Jan Kowalski
```
Możemy także konkatenować łańcuchy w miejscu, bez potrzeby tworzenia nowych zmiennych:
```Swift
var fullName = "Jan"
fullName += " Kowalski"
print(fullName)
// Output: Jan Kowalski
```
Kolejną przydatną funkcją jest interpolacja łańcuchów, czyli wstawianie wartości zmiennych bezpośrednio do tekstu. W tym celu używamy znaku "\\" przed nazwą zmiennej wewnątrz łańcucha.
```Swift
let points = 50
let message = "Gratulacje! Zdobyłeś \(points) punktów."
print(message)
// Output: Gratulacje! Zdobyłeś 50 punktów.
```
Warto również pamiętać o używaniu opcji string interpolation (Interpolacji łańcuchów) zamiast konkatenacji, jeśli potrzebujemy wstawiać więcej niż 2-3 zmienne do naszego łańcucha.

## Deep Dive

Podczas konkatenacji łańcuchów należy uważać na wydajność naszego kodu. Jeśli do naszej zmiennej typu String będziemy dodawać kolejne łańcuchy w pętli, każdy z tych łańcuchów będzie tworzony od nowa, co jest bardzo nieefektywne. W takim przypadku lepiej korzystać z typu NSMutableString, który pozwala na modyfikację jednego łańcucha bez potrzeby tworzenia nowych.

Należy również pamiętać o używaniu odpowiednich separatorów, takich jak spacja czy przecinek, w celu poprawnego wyświetlenia danych. Przykładowo, jeśli nie dodamy spacji między imieniem a nazwiskiem, nasz łańcuch może wyglądać niepoprawnie.

## Zobacz także

- Dokumentacja Swift dotycząca konkatenacji łańcuchów (https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID290)
- Poradnik "Jak efektywnie korzystać z konkatenacji w Swift" (https://www.appcoda.com/swift-string-concatenation/)
- Wideo tutorial "Swift Basics: String Concatenation and Interpolation" (https://www.youtube.com/watch?v=iE5hh2aY50U)