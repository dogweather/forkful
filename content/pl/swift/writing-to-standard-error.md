---
title:                "Pisanie do standardowego błędu"
html_title:           "Swift: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego
Standardowy błąd, znany w języku Swift jako standardowy strumień błędów, jest jednym ze sposobów, w jaki możemy rejestrować ważne informacje o działaniu naszego programu. Dzięki temu możemy lepiej zrozumieć bugi i problemy, jakie mogą pojawić się podczas jego wykonywania.

## Jak to zrobić
Aby wypisać informację do standardowego błędu, wystarczy użyć metody `print()` z dodatkowym parametrem `to:`. Przykładowo:

```Swift
let message = "Błąd: nie można otworzyć pliku"
print(message, to: &Stderr.standardError)
```

Wywołanie tej metody spowoduje wydrukowanie wiadomości do standardowego błędu, więc będzie ona widoczna w konsoli lub zapisana do pliku dziennika.

## Głębsza analiza
Pisanie do standardowego błędu jest szczególnie użyteczne w sytuacjach, gdy nie chcemy przerywać wykonywania programu przy napotkaniu błędu, ale jednocześnie chcielibyśmy go zarejestrować i poinformować użytkownika. Możemy również użyć tej metody do wyświetlania dodatkowych informacji diagnostycznych podczas testowania lub debugowania aplikacji.

## Zobacz również
- [Dokumentacja Swift: Standardowy strumień błędów](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html#ID517)
- [Przykład użycia metody print() z parametrem to:](https://stackoverflow.com/questions/36751903/what-is-with-stderr-in-swift)