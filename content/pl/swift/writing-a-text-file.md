---
title:    "Swift: Tworzenie pliku tekstowego"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Dlaczego

Pisanie pliku tekstowego jest jednym z podstawowych elementów tworzenia aplikacji w języku Swift. Jest to często używana metoda do zapisywania danych lub wyników działania programu. Poniżej przedstawiamy wskazówki, jak napisać plik tekstowy w prosty i efektywny sposób.

## Jak to zrobić

Kodowanie pliku tekstowego w języku Swift jest proste i wymaga tylko kilku linii kodu. Najpierw musimy zadeklarować zmienną typu String, która będzie zawierać nasz tekst. Następnie tworzymy ścieżkę do naszego pliku przy użyciu metody URL.

```Swift
let text = "Cześć! Witaj w moim blogu!"
let fileURL = URL(fileURLWithPath: "/Users/janek/Desktop/blog.txt")
```

Następnie, musimy utworzyć obiekt klasy FileManager, który będzie odpowiedzialny za manipulację plikami. Wykorzystujemy go do utworzenia naszego pliku przy użyciu metody `createFile()`. 

```Swift
let fileManager = FileManager.default
fileManager.createFile(atPath: fileURL.path, contents: nil, attributes: nil)
```

Następnie, z pomocą obiektu `FileHandle`, który reprezentuje otwarty plik, możemy zapisać nasz tekst za pomocą metody `write()`. Pamiętajmy, że musimy go zamknąć po zapisaniu danych.

```Swift
let fileHandle = try FileHandle(forWritingTo: fileURL)
fileHandle.write(text.data(using: .utf8)!)
fileHandle.closeFile()
```

## Głębszy zanurzenie

Powyższe przykłady pokazują podstawowe sposoby na zapisanie tekstu do pliku tekstowego, ale istnieje wiele innych metod i funkcji, dzięki którym możemy bardziej zaawansowanie zarządzać naszym plikiem. Na przykład, możemy wykorzystać funkcję `append()` do dodawania tekstu do już istniejącego pliku, a także korzystać z różnych typów enkodera, aby dopasować format zapisywanych danych.

## Zobacz też

- Dokumentacja Apple Developer: [Working with File System](https://developer.apple.com/documentation/foundation/file_manager)
- Oryginalny artykuł w języku angielskim: [Writing a Text File in Swift](https://www.programiz.com/swift-programming/write-text-file)