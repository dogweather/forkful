---
title:                "Czytanie pliku tekstowego"
html_title:           "C: Czytanie pliku tekstowego"
simple_title:         "Czytanie pliku tekstowego"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Odczytywanie pliku tekstowego to proces wydobywania danych z zapisanych wcześniej plików tekstowych. Programiści robią to, aby móc manipulować, analizować i wykorzystywać te dane w swojem kodzie.

## Jak to zrobić:

Oto przykład, jak odczytać plik tekstowy w Swift:

```Swift
import Foundation

let fileURL = Bundle.main.url(forResource: "Example", withExtension: "txt")
do {
    let content = try String(contentsOf: fileURL!, encoding: .utf8)
    print(content)
} catch {
    print("Cannot read the file")
}
```

W tym przypadku, jeśli plik "Example.txt" znajduje się w głównym pakiecie i zawiera linijkę "Hello, Swift!", wynikiem będzie:

```Swift
Hello, Swift!
```

## Deep Dive

Często odczytywanie plików tekstowych bywa konieczne, np. przy analizie danych czy konfiguracji aplikacji. Istnieje wiele sposobów na odczyt plików tekstowych, a Swift oferuje prosty i intuicyjny interfejs do tej operacji. Historia plików tekstowych sięga czasów Unixa i do dziś są one znaczącym elementem komputacji. Co do szczegółów implementacji, Swift wykorzystuje typ String do przechowywania zawartości pliku. 

## Zobacz również 

1. Dokumentacja Swift: [String](https://developer.apple.com/documentation/swift/string)
2. Jak odczytać plik w Swift: [StackOverflow](https://stackoverflow.com/questions/24581517/read-a-file-url-line-by-line-in-swift)
3. Więcej o plikach tekstowych: [Wikipedia](https://pl.wikipedia.org/wiki/Plik_tekstowy)