---
title:    "Swift: Tworzenie pliku tymczasowego"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie tymczasowego pliku może być niezbędnym krokiem w pisaniu aplikacji w języku Swift. Służy on do przechowywania tymczasowych danych lub do tymczasowego wykonywania pewnych operacji. Jest to również przydatna funkcja, gdy potrzebujemy przechować pobrane dane, ale nie mamy stałego miejsca do ich zapisu.

## Jak To Zrobić

Aby utworzyć tymczasowy plik w Swift, możemy użyć następującej metody:

```Swift
let temporaryDirectory = NSTemporaryDirectory()
let temporaryFileURL = URL(fileURLWithPath: temporaryDirectory).appendingPathComponent("temporaryFile").appendingPathExtension("txt")

do {
    try "To jest przykładowy tekst".write(to: temporaryFileURL, atomically: true, encoding: .utf8)
    print("Plik tymczasowy został utworzony!")
} catch {
    print("Wystąpił błąd podczas tworzenia pliku: \(error)")
}
```

W powyższym przykładzie używamy klasy `NSTemporaryDirectory()`, aby uzyskać ścieżkę do katalogu tymczasowego na urządzeniu. Następnie tworzymy adres URL dla naszego pliku tymczasowego, do którego będziemy mogli pisać nasze dane.

Po utworzeniu pliku możemy go zapisać używając metody `write`, która przyjmuje trzy parametry: zawartość pliku, ścieżkę do pliku i kodowanie. W przypadku, gdy wszystko przebiegnie pomyślnie, zostanie wyświetlony komunikat potwierdzający jego utworzenie.

## Deep Dive

Tworzenie tymczasowego pliku w Swift jest możliwe dzięki wykorzystaniu klas z `Foundation`. W tym przypadku, korzystamy z klasy `URL`, która pozwala na tworzenie adresów URL do plików. Istotną częścią tego procesu jest również wykorzystanie mechanizmu `do-catch`, który pozwala na obsługę ewentualnych błędów podczas operacji na pliku.

Dodatkowo, w przeciwieństwie do klasycznych metod tworzenia plików, wykorzystanie mechanizmu tymczasowego pozwala na automatyczne usuwanie pliku po zakończeniu programu lub procesu.

## Zobacz też

- [Oficjalna dokumentacja Swift](https://swift.org/documentation/)
- [Poradnik tworzenia aplikacji w języku Swift](https://learnappmaking.com/create-a-swift-app-xcode/)
- [Wykorzystywanie klas z `Foundation`](https://www.hackingwithswift.com/read/10/overview)