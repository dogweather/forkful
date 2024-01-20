---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Arduino: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Długość ciągu to liczba znaków, które go tworzą. Programiści muszą to znać, aby operować i manipulować ciągami – czy to wyświetlanie tekstu, przetwarzanie danych, czy analiza tekstu.

## Jak to zrobić:

W Swift, używamy właściwości `count` na ciągu, aby uzyskać jego długość. Oto jak to zrobić: 

```Swift
let str = "Cześć, Swift!"
let length = str.count
print(length)  // Wynik: 13
```

Te dwie linie kodu zwrócą długość ciągu "Cześć, Swift!", która wynosi 13.

## Pogłębiona analiza

Długość ciągu była zawsze istotnym aspektem dla programistów, ponieważ na podstawie długości ciągów wykonują różne operacje. W Swift, `count` zwraca liczbę "znaków wyświetlanych", co może być różne dla różnych języków i systemów.

Jako alternatywę, możesz używać metody `utf16.count` lub `utf8.count` do uzyskania długości ciągu, ale to zależy od konkretnego zastosowania i żądanych szczegółów implementacji.

## Zobacz także

* Dokumentacja Apple o ciągach: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html
* Post na StackOverflow o długości ciągów: https://stackoverflow.com/questions/24092886/get-length-of-string-in-swift