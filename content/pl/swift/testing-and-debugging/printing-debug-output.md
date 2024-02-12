---
title:                "Drukowanie komunikatów debugowania"
aliases:
- /pl/swift/printing-debug-output.md
date:                  2024-01-20T17:53:25.994422-07:00
model:                 gpt-4-1106-preview
simple_title:         "Drukowanie komunikatów debugowania"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wypisywanie danych debugowych to jak rozmowa z komputerem – wysyłasz mu pytanie i czekasz na odpowiedź. Programiści to robią, aby zrozumieć, co dzieje się w aplikacji podczas działania i znaleźć błędy.

## Jak to zrobić:
W Swift, wykorzystuje się funkcję `print()` do wypisywania danych debugowych w konsoli. Prosty przykład:

```Swift
let debugMessage = "To jest debug"
print(debugMessage)
// Wyświetli: To jest debug
```

Chcesz więcej szczegółów? Użyj interpolacji stringów:

```Swift
let currentValue = 5
print("Wartość zmiennej: \(currentValue)")
// Wyświetli: Wartość zmiennej: 5
```

## Deep Dive
Wypisywanie informacji do konsoli jest tak stare jak samo programowanie. W latach 70., programiści używali teletypów do komunikacji z komputerami. Oczywiście, Swift wprowadził znaczne ulepszenia. Alternatywą dla `print()` może być `debugPrint()`, który daje więcej informacji typowych dla debugowania, lub używanie logów z pomocą `os_log` w aplikacjach dla macOS i iOS, co pozwala na lepsze zarządzanie logami w systemie.

Swift umożliwia także definiowanie własnych operatorów do wypisywania, co może być przydatne jeżeli chcesz skonfigurować precyzyjne formatowanie:

```Swift
infix operator >>>

func >>> (message: String, function: String) {
    print("[\(function)]: \(message)")
}

let error = "Błąd połączenia"
error >>> #function
// Przykładowo wyświetli: [tuNazwaFunkcji]: Błąd połączenia
```

Zwróć uwagę, że nadużywanie `print()` może spowolnić aplikację i utrudnić czytanie logów, więc stosuj to z głową.

## Zobacz także
- [Dokumentacja Swift dla `print()` function](https://developer.apple.com/documentation/swift/1541053-print)
- [Różnice między `print()` a `debugPrint()` w Swift](https://www.hackingwithswift.com/example-code/language/whats-the-difference-between-print-and-debugprint)
- [Apple Documentation on `os_log`](https://developer.apple.com/documentation/os/logging)
