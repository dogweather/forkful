---
title:    "Kotlin: Tworzenie pliku tymczasowego"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego
Tworzenie tymczasowych plików jest nieodłączną częścią wielu aplikacji i programów w Kotlinie. Czy chcesz zapisać tymczasowe dane lub pobierać dane z zewnętrznych źródeł, tymczasowe pliki mogą być bardzo użyteczne w tym procesie.

## Jak to zrobić
Kotlin ma wbudowaną funkcjonalność do tworzenia tymczasowych plików za pomocą `createTempFile()` metody. Możesz wybrać swoją własną ścieżkę oraz prefiks i sufiks dla tymczasowego pliku, aby lepiej go zidentyfikować. Oto przykładowy kod:

```Kotlin
val tempFile = createTempFile(prefix = "temp", suffix = ".txt")
tempFile.writeText("To jest przykładowy zawartość tymczasowego pliku")
println(tempFile.absolutePath)
```

Jeśli uruchomisz ten kod, zobaczysz następujący wynik:

```Kotlin
C:\Users\User\AppData\Local\Temp\temp123456789.txt
```

Tymczasowy plik zostanie automatycznie usunięty, gdy program zakończy działanie lub gdy odwołasz się do `delete()` metody na obiekcie `tempFile`.

## Głębszy wgląd
Podczas tworzenia tymczasowego pliku możesz również zdefiniować katalog, w którym chcesz go utworzyć, za pomocą opcjonalnego parametru `directory`. Jeśli nie podasz żadnej wartości, plik zostanie stworzony w domyślnym folderze systemowym dla tymczasowych plików.

Podczas pracy z tymczasowymi plikami musisz pamiętać, że informacje w nich zawarte mogą zostać utracone, dlatego nie są zalecane do długotrwałego przechowywania danych.

## Zobacz też
- Dokumentacja oficjalna Kotlina: https://kotlinlang.org/docs/reference/io-files.html#working-with-temporary-files
- Inne przydatne funkcje Kotlina: https://kotlinlang.org/docs/reference/idioms.html