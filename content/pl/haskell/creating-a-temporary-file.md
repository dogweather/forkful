---
title:                "Tworzenie tymczasowego pliku"
html_title:           "C#: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Tworzenie tymczasowego pliku polega na stworzeniu pliku, który jest używany na czas określonej operacji lub sesji, i który zwykle jest usuwany po zakończeniu. Programiści tworzą takie pliki, aby tymczasowo przechowywać dane bez potrzeby zapisywania ich w stałej pamięci.

## Jak to zrobić:

W Haskellu możesz użyć `System.IO.Temp` do stworzenia tymczasowego pliku. Zobaczmy szybki przykład:

```haskell
import System.IO.Temp
import System.IO

main = withSystemTempFile "mytemp.txt" $ \_ handle -> hPutStr handle "Cześć, jestem tymczasowy!"

```

Oto wysyłane dane:

```txt
Cześć, jestem tymczasowy!
```

## W głąb:
 
Tworzenie tymczasowych plików w programowaniu ma kilku dekad historii. Znaczenie tego wzrosło wraz z pojawieniem się systemów operacyjnych z wielozadaniowością i rosnącym zapotrzebowaniem na pamięć podręczną.

Alternatywą dla lokalnych plików tymczasowych mogą być pliki tymczasowe przechowywane w pamięci, z której korzystają niektóre bazy danych i systemy plików, ale mogą one być mniej wydajne.

Zgodnie z dokumentacją `System.IO.Temp`, Haskell tworzy tymczasowy plik w folderze określonym przez środowisko. Plik zostaje usunięty gdy uchwyt do pliku zostanie zamknięty, chyba że podasz wyraźnie inną instrukcję.

## Zobacz również:

Haskell ma wiele bibliotek do zarządzania plikami i folderami. Sprawdź je: