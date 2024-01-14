---
title:    "Swift: Tworzenie pliku tymczasowego"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie tymczasowych plików jest nieodłączną częścią programowania. Służą one do przechowywania danych, które są potrzebne tylko na krótki czas i nie są uważane za ważne do stałego przechowywania. W Swift, istnieje kilka sposobów na tworzenie tymczasowych plików, a w tym artykule dowiesz się jak to zrobić.

## Jak to zrobić

Tworzenie tymczasowych plików w Swift jest bardzo proste. Możesz to zrobić za pomocą kilku linijek kodu. Wykorzystajemy do tego klasę `URL` i metodę `URLByAppendingPathComponent`:

```Swift
let temporaryURL = URL(fileURLWithPath: NSTemporaryDirectory()).appendingPathComponent("temporaryFile").appendingPathExtension("txt")

print(temporaryURL.path) // /var/folders/4b/q2f0g5g17vjf2ybxkk0mj8q80000gn/T/temporaryFile.txt
```

W powyższym przykładzie, najpierw definiujemy zmienną `temporaryURL` używając klasy `URL` i metody `fileURLWithPath` z argumentem `NSTemporaryDirectory()`, dzięki czemu dostajemy ścieżkę do tymczasowego katalogu. Następnie, za pomocą metody `appendingPathComponent`, dodajemy nazwę pliku oraz za pomocą `appendingPathExtension` nadajemy mu rozszerzenie. W końcu, możemy wydrukować ścieżkę tymczasowego pliku używając właściwości `path`.

Możesz również utworzyć tymczasowy plik bez wykorzystywania metody `NSTemporaryDirectory()`:

```Swift
let temporaryURL = URL(fileURLWithPath: "temporaryFile").appendingPathExtension("txt")

print(temporaryURL.path) // /temporaryFile.txt
```

## Deep Dive

W Swift, możesz również użyć klasy `FileManager` do utworzenia tymczasowego pliku. Poniższy kod przedstawia przykład:

```Swift
let fileManager = FileManager.default
let temporaryURL = URL(fileURLWithPath: NSTemporaryDirectory()).appendingPathComponent("temporaryFile").appendingPathExtension("txt")

do {
    try fileManager.createFile(at: temporaryURL, contents: nil, attributes: nil)
    print("Plik tymczasowy utworzony z sukcesem!")
} catch {
    print("Nie udało się utworzyć pliku: \(error)")
}
```

Powyższy kod używa metody `createFile` z klasy `FileManager`, która pozwala nam utworzyć pusty plik na wybranej przez nas ścieżce. Jeśli chcesz również umieścić dane w tworzonym pliku, można wykorzystać argument `contents` i przekazać do niego odpowiednie dane.

## Zobacz też

- [Dokumentacja Swift – Tworzenie plików](https://docs.swift.org/swift-book/LanguageGuide/FilesAndDirectories.html)
- [Kurs Swift – Zarządzanie plikami i katalogami](https://www.ralfebert.de/ios/tutorials/filemanager/)
- [Medium – Tworzenie tymczasowych plików w Swift](https://medium.com/@PavloTymoshevsky/swift-create-a-temporary-file-and-use-it-34c7868c0f6f)