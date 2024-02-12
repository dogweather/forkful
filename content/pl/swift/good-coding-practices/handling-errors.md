---
title:                "Obsługa błędów"
aliases:
- /pl/swift/handling-errors.md
date:                  2024-01-26T00:58:14.105685-07:00
model:                 gpt-4-1106-preview
simple_title:         "Obsługa błędów"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/handling-errors.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Obsługa błędów w Swift polega na przewidywaniu i reagowaniu na problemy, które pojawiają się podczas wykonywania Twojego kodu. Robimy to, aby kontrolować chaos – zapobiegamy awariom aplikacji i zapewniamy użytkownikowi płynne korzystanie z niej.

## Jak to zrobić:
Swift wykorzystuje obsługę błędów za pomocą bloków `do`, `try` i `catch`. Spójrzmy na to:

```Swift
enum FileError: Error {
    case fileDoesNotExist
    case noPermission
}

func readFile(atPath path: String) throws -> String {
    // Załóżmy, że mamy tutaj jakąś logikę sprawdzającą, czy plik istnieje i czy mamy uprawnienia do jego odczytu
    let fileExists = false
    let havePermission = true

    if !fileExists {
        throw FileError.fileDoesNotExist
    }

    if !havePermission {
        throw FileError.noPermission
    }

    return "Zawartość pliku znajduje się tutaj"
}

do {
    let fileContent = try readFile(atPath: "/ścieżka/do/pliku")
    print(fileContent)
} catch FileError.fileDoesNotExist {
    print("Ups! Plik nie został znaleziony.")
} catch FileError.noPermission {
    print("A! Brak uprawnień do odczytu pliku.")
} catch {
    print("Wystąpił nieznany błąd.")
}

```

Przykładowe wyjście:

```
Ups! Plik nie został znaleziony.
```

## Glebsze zanurzenie
Obsługa błędów nie zawsze była tak wyrafinowana jak teraz. W Objective-C miałeś do czynienia z wskaźnikami do obiektów NSError, co wydawało się niezgrabne. Teraz mamy bardziej elegancki system z enumami Swift i protokołem `Error`.

`throw` w Swifcie pozwala nam sygnalizować, że coś poszło nie tak. Bloki `do` działają jak obszary świadome błędów, `try` oznacza ryzykowną operację, a `catch` radzi sobie z sytuacjami, gdy coś pójdzie nie tak.

Opcjonalne (ang. optionals) to alternatywa dla sytuacji, które nie są do końca "stanem błędu", ale mogą nadal nie mieć "wyniku". Są trochę jak zmienne Schrödingera – mają wartość albo jej nie mają.

Dla prawdziwej głębi zbadaj typy `Result`, które są świetnymi hybrydami między standardowym zwracaniem wyników a wzorcami błędów.

## Zobacz również
- Oficjalny przewodnik po obsłudze błędów Swift: [Apple Docs](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- Najlepsze praktyki obsługi błędów w Swifcie: [RayWenderlich.com](https://www.raywenderlich.com/1851-beginning-swift-error-handling)
- Zaawansowana obsługa błędów w Swifcie: [artykuł na Medium](https://medium.com/better-programming/advanced-error-handling-in-swift-4f6bdf6b01d8)
