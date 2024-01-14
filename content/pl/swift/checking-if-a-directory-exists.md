---
title:                "Swift: Sprawdzanie, czy istnieje katalog"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzenie, czy katalog istnieje, jest niezbędnym krokiem w wielu projektach programistycznych. Może to pomóc w uniknięciu błędów, które mogą wystąpić, gdy próbujemy korzystać z nieistniejącego katalogu. W tym wpisie dowiesz się, dlaczego warto znać ten podstawowy krok w programowaniu w języku Swift.

## Jak to zrobić

Aby sprawdzić, czy dany katalog istnieje, możesz użyć metody "fileExists" na obiekcie FileManager. Przykładowy kod wyglądałby następująco:

```Swift
let fileManager = FileManager.default
let directoryURL = URL(fileURLWithPath: "/Users/janek/Desktop") // podaj ścieżkę do katalogu, który chcesz sprawdzić
var isDirectory: ObjCBool = false

if fileManager.fileExists(atPath: directoryURL.path, isDirectory: &isDirectory) {
    if isDirectory.boolValue {
        print("Katalog istnieje")
    } else {
        print("To nie jest katalog")
    }
} else {
    print("Katalog nie istnieje")
}
```

Wywołanie tej metody zwraca wartość logiczną, która informuje nas o tym, czy katalog istnieje. Możemy także użyć parametru "isDirectory", aby sprawdzić, czy dana ścieżka jest faktycznie katalogiem czy też plikiem.

## Głębsze zagłębienie

Sprawdzanie, czy katalog istnieje, może być także pomocne w procesie tworzenia aplikacji, które wymagają dostępu do określonych katalogów. Na przykład, jeśli tworzysz aplikację do przechowywania zdjęć, musisz mieć dostęp do katalogu zdjęć użytkownika. Dzięki wykorzystaniu metody "fileExists", możesz upewnić się, że aplikacja jest w stanie odnaleźć i uzyskać dostęp do tego katalogu.

Inną przydatną metodą związaną z tym tematem jest "createDirectory", która pozwala na tworzenie nowych katalogów w wybranym miejscu. Jest to szczególnie użyteczne, jeśli musisz stworzyć nowy katalog dla swojej aplikacji lub zapisać dane użytkownika w wybranym katalogu.

## Zobacz też

- [Dokumentacja Apple na temat zarządzania plikami](https://developer.apple.com/documentation/foundation/file_management)
- [Przykładowy projekt z wykorzystaniem operacji na plikach w języku Swift](https://github.com/marekuliasz/FileManagerExample)