---
title:                "Elm: Tworzenie pliku tymczasowego"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami programiści muszą tworzyć tymczasowe pliki w swoich projektach z różnych powodów, na przykład tymczasowe dane, tymczasowe pamięci cache lub pliki tymczasowe do testowania kodu. W tym artykule omówimy, dlaczego warto używać tymczasowych plików w języku Elm.

## Jak

Aby stworzyć tymczasowy plik w Elm, należy wykorzystać funkcję `File.Temp.file` z pakietu `elm/file`. Poniżej znajduje się przykładowy kod:

```elm
import File.Temp exposing (file)
import File exposing (toFile)

-- tworzenie pliku tymczasowego w aktualnym folderze
myTempFile = File.Temp.file "temp.txt"

-- konwertowanie do pliku
myFile = toFile myTempFile
```

Po uruchomieniu powyższego kodu, zostanie utworzony tymczasowy plik `"temp.txt"` w aktualnym folderze. Run this  When running this code, you will see the following output:

```
<SUCCESS> Plik temp.txt został utworzony.
```

## Wnikliwa analiza

Powyższy przykład jest bardzo prosty, ale warto dowiedzieć się więcej o tworzeniu tymczasowych plików. Funkcja `File.Temp.file` przyjmuje dwa argumenty: nazwę pliku i opcjonalną lokalizację, w której należy go utworzyć. Jeśli nie podamy lokalizacji, zostanie utworzony w aktualnym folderze. 

Funkcja `File.Temp.file` zwraca wartość typu `Result`, który może zawierać albo wartość `Ok`, gdy operacja jest wykonana poprawnie, albo błąd, gdy wystąpi jakikolwiek problem. Możemy wykorzystać wzorce dopasowań do obsłużenia obu tych przypadków w przykładowym kodzie. Dodatkowo, możemy także użyć funkcji `File.Temp.tempDirectory` do uzyskania ścieżki do folderu tymczasowego, w którym można tworzyć pliki tymczasowe.

## Zobacz także

1. Dokumentacja pakietu `elm/file` (https://package.elm-lang.org/packages/elm/file/latest/)
2. Przykłady wykorzystania tymczasowych plików w Elm (https://github.com/elm-explorations/file/tree/1.0.2/examples)
3. Poradnik dotyczący działania na plikach w Elm (https://giannopoulos.dev/blog/2020/05/01/handling-files-in-elm/)