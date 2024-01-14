---
title:                "Swift: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego?

Jeśli potrzebujesz łatwego sposobu na przechowywanie i przetwarzanie danych w formacie tabeli, pliki CSV mogą okazać się bardzo przydatne. W tym artykule dowiesz się, dlaczego warto pracować z CSV i jak to zrobić w języku Swift.

## Jak to zrobić?

Do rozpoczęcia pracy z plikami CSV w języku Swift potrzebne są tylko trzy kroki: otwarcie pliku, odczytanie danych i zamknięcie pliku. W poniższym przykładzie użyjemy danych dotyczących książek.

```Swift
import Foundation
let url = URL(fileURLWithPath: "books.csv")

do {
    let data = try Data(contentsOf: url)
    guard let books = String(data: data, encoding: .utf8) else { return }
    print(books)
} catch {
    print(error)
}
```

Output:
```
Title, Author, Genre, Year
To Kill a Mockingbird, Harper Lee, Classic, 1960
Pride and Prejudice, Jane Austen, Romance, 1813
1984, George Orwell, Dystopian, 1949
```

W powyższym kodzie najpierw importujemy framework Foundation, który zawiera potrzebne metody do obsługi plików. Następnie tworzymy URL dla naszego pliku CSV i używamy go do wczytania danych. W linii trzeciej konwertujemy dane do formatu String za pomocą metody init(data:encoding:) i wypisujemy je na ekran. W przypadku błędu wyświetlamy go na konsoli.

## Deep Dive

Tworząc aplikację obsługującą pliki CSV, warto również zwrócić uwagę na różne separatora i znaki specjalne, które mogą wpłynąć na poprawność odczytu danych. Na przykład, jeśli wartości w danym polu są oddzielone przecinkiem, a jednocześnie przecinek występuje również w środku tekstu, to nasz program może źle zinterpretować dane.

Dodatkowo, w przypadku dużych plików CSV, ważne jest uważne zarządzanie pamięcią, aby uniknąć przeciążenia i awarii aplikacji.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o pracy z plikami CSV w języku Swift, koniecznie zajrzyj na poniższe strony:

- [Dokumentacja Apple na temat klasy String](https://developer.apple.com/documentation/swift/string)
- [Poradnik o pracy z plikami CSV w Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Biblioteka SwiftCSV do łatwego i szybkiego przetwarzania plików CSV](https://github.com/swiftcsv/SwiftCSV)