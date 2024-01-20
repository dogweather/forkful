---
title:                "Łączenie ciągów znaków"
html_title:           "Arduino: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Łączenie stringów to proces łączenia dwóch lub więcej ciągów znaków w jeden dłuższy. Programiści wykonują to, aby tworzyć dynamiczne wiadomości i wyniki w programach.

## Jak to zrobić:

W Swift, łatwo łączy się stringi za pomocą operatora "+". Poniżej przedstawiam przykładowy kod:
```swift
let ciag1 = "Cześć, "
let ciag2 = "jaki jest twój numer telefonu?"
let polaczonyCiag = ciag1 + ciag2
print(polaczonyCiag)
```
Po wykonaniu tego kodu na wyjściu otrzymasz:
```swift
"Cześć, jaki jest twój numer telefonu?"
```
Możesz również interpolować stringi, co pozwala na ich łączenie na bieżąco, jak w poniższym przykładzie:
```swift
let imie = "Jan"
let powitanie = "Cześć, \(imie)"
print(powitanie)
```
Wynik:
```swift
"Cześć, Jan"
```
## Głębszy Wgląd

Łączenie stringów jest koncepcją, która sięga początków programowania. Wszystkie języki mają swoje sposoby na realizację tej funkcji, ale Swift robi to w bardziej syntetyczny i czytelny sposób.

Alternatywą dla łączenia stringów mogłby być tworzenie kompleksowych struktur danych stringów, ale jest to zazwyczaj bardziej pracochłonne i złożone.

Szczegół implementacji: pod spodem, kiedy łączysz stringi, Swift efektywnie tworzy nową instancję stringu, która zawiera obie łączone wartości. To jest różne od niektórych innych języków, które mogą po prostu wskazywać na oryginalne stringi.

## Zobacz również
- Oficjalna dokumentacja Swift:'String' (https://developer.apple.com/documentation/swift/string)
- Artykuł Swifts' String Manifesto' (https://github.com/apple/swift/blob/master/docs/StringManifesto.md)
- Interpolacja Stringów (https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID292)