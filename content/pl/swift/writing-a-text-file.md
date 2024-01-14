---
title:    "Swift: Tworzenie pliku tekstowego"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie plików tekstowych jest nieodzowną częścią programowania w Swift. Może to służyć do przechowywania danych, tworzenia konfiguracji lub przygotowania aplikacji do użycia. Dzięki napisaniu tekstu program można w prosty sposób manipulować danymi wewnątrz aplikacji.

## Jak to zrobić

Poniżej przedstawiono prosty przykład kodu w języku Swift, który tworzy plik tekstowy o nazwie "example.txt" w katalogu dokumentów na urządzeniu. W tym przypadku plik zawiera prosty tekst "Hello World!".

```Swift
let text = "Hello World!"

let documentsDirectory = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!
let fileURL = documentsDirectory.appendingPathComponent("example.txt")

do {
    try text.write(to: fileURL, atomically: true, encoding: .utf8)
} catch {
    print("Error writing text file: \(error)")
}
```

Po uruchomieniu powyższego kodu, w katalogu dokumentów pojawi się nowo utworzony plik "example.txt" zawierający tekst "Hello World!".

## Wnikliwe spojrzenie

Pisanie plików tekstowych może być bardziej skomplikowane niż w przykładzie powyżej, zwłaszcza jeśli wymaga obsługi większej ilości danych. W takim przypadku przydatne może być użycie pętli lub funkcji służących do odczytu i zapisu danych z pliku.

Pamiętaj również, aby użyć odpowiednich kodowań, jeśli plik tekstowy zawiera znaki spoza standardowego zestawu znaków. W przypadku braku takiej informacji, plik może zostać nieprawidłowo odczytany.

## Zobacz też

- [Oficjalna dokumentacja Swift na temat tworzenia i edycji plików tekstowych](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID293)
- [Przewodnik po plikach i katalogach w języku Swift](https://www.hackingwithswift.com/read/0/overview)
- [Przydatny artykuł na temat obsługi plików w języku Swift](https://learnappmaking.com/read-write-files-swift-programming/)