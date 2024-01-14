---
title:                "Swift: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Dlaczego warto tworzyć tymczasowe pliki?

Stworzenie tymczasowego pliku jest ważne w przypadku, gdy potrzebujemy przechować tymczasowe dane w naszej aplikacji. Może to być przydatne, gdy chcemy pobierać pliki z Internetu lub przeprowadzać inne obliczenia, które wymagają obecności tymczasowego pliku. W tym artykule dowiesz się, jak w prosty sposób stworzyć taki plik w języku Swift.

## Jak to zrobić?

W celu stworzenia tymczasowego pliku w języku Swift, możemy skorzystać z klasy `FileManager`. W pierwszym kroku musimy utworzyć ścieżkę do naszego pliku, który chcemy stworzyć. Możemy to zrobić za pomocą metody `url(for:in:appropriateFor:create:)`, przekazując jako parametry odpowiednią nazwę pliku oraz lokalizację, w której chcemy go stworzyć.

```Swift
let temporaryFileName = "temporaryFile.txt"
let temporaryURL = FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask).first!.appendingPathComponent(temporaryFileName)
```

Następnie możemy wykorzystać metodę `createFile(atPath:contents:attributes:)` w celu stworzenia pliku na wybranej ścieżce. W przykładzie poniżej, tworzymy tymczasowy plik, do którego zapisujemy tekst "Hello World!".

```Swift
FileManager.default.createFile(atPath: temporaryURL.path, contents: "Hello World!".data(using: .utf8), attributes: nil)
```

Po wykonaniu tych kroków, mamy już gotowy tymczasowy plik w wybranej lokalizacji. Możemy go teraz wykorzystać do naszych potrzeb.

## Głębszy wgląd

Istnieje wiele różnych metod w klasie `FileManager`, które pozwalają nam zarządzać plikami w systemie. Możemy na przykład sprawdzić, czy plik istnieje w danej lokalizacji za pomocą metody `fileExists(atPath:)` lub skopiować lub przenieść plik z jednej lokalizacji do drugiej przy pomocy odpowiednio metod `copyItem(atPath:toPath:)` lub `moveItem(atPath:toPath:)`.

Pamiętaj jednak, że plik tymczasowy zostanie usunięty po zamknięciu aplikacji. Jeśli chcesz, aby plik był dostępny nawet po ponownym uruchomieniu aplikacji, musisz go samodzielnie skopiować do stałej lokalizacji lub użyć innych metod zarządzania plikami.

## Zobacz też

Zapoznaj się z innymi narzędziami dostępnymi w języku Swift, które pozwalają na zarządzanie plikami:

- [Obsługa plików w języku Swift](https://developer.apple.com/documentation/foundation/file_access_and_management)
- [FileManager - dokumentacja Apple](https://developer.apple.com/documentation/foundation/filemanager)

Pamiętaj, że stworzenie tymczasowego pliku może być bardzo przydatne w wielu sytuacjach. Dzięki temu nie musimy martwić się o przechowywanie tymczasowych danych i możemy skupić się na naszym programowaniu.