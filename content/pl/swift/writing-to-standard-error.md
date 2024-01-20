---
title:                "Pisanie do standardowego błędu"
html_title:           "Arduino: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Pisanie do standardowego błędu (stderr) pozwala separować normalne wyjście programu od komunikatów o błędach. Programiści robią to, aby ułatwić obsługę błędów i logowanie, szczególnie przy przetwarzaniu potokowym i przekierowaniach.

## Jak to zrobić:
```Swift
import Foundation

// Przykład zapisu do standardowego błędu (stderr)
func logError(message: String) {
    if let data = "\(message)\n".data(using: .utf8) {
        FileHandle.standardError.write(data)
    }
}

// Użycie funkcji
logError(message: "Wystąpił błąd!")
```
Przykładowe wyjście w konsoli (wyświetlenie błędu):
```
Wystąpił błąd!
```

## Zanurkujmy głębiej
Pisząc do `stderr`, oddzielamy normalne dane wyjściowe programu od komunikatów o błędach, co ma swoje korzenie w systemach uniksowych, gdzie podzielono te strumienie, aby umożliwić ich niezależne przetwarzanie. Alternatywą dla `stderr` jest `stdout`, ale używamy go do przekazywania wyników działania programu, nie błędów. Szczegół realizacji opiera się na zarządzaniu deskryptorami plików – `stderr` ma zazwyczaj deskryptor 2.

## Zobacz również
- [Dokumentacja Swift na temat FileHandle](https://developer.apple.com/documentation/foundation/filehandle)
- [POSIX Standard: Standard Error - opis standardu](https://pubs.opengroup.org/onlinepubs/9699919799/functions/stdin.html)