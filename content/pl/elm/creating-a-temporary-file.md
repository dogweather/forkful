---
title:                "Tworzenie pliku tymczasowego"
html_title:           "Elm: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego
Gdy pracujemy z programowaniem, często potrzebujemy tworzyć tymczasowe pliki w celu przechowania danych lub jako etap pośredni w przetwarzaniu informacji. W takich sytuacjach, ważne jest aby mieć wydajne i niezawodne narzędzie do tworzenia tymczasowych plików. W tym artykule przedstawimy jak rozwiązać ten problem przy użyciu języka programowania Elm.

## Jak to zrobić
Tworzenie tymczasowego pliku w Elm jest bardzo proste. Wystarczy użyć funkcji "File.createTemp" podając ścieżkę, w której chcemy utworzyć plik, oraz jego nazwę. Przykładowy kod wygląda następująco:

```Elm
File.createTemp "tmp/" "plik.docx"
```

Rezultatem będzie utworzenie pliku o nazwie "plik.docx" w folderze "tmp". Możliwe jest również podanie pełnej ścieżki w celu utworzenia pliku w konkretnym miejscu.

Aby upewnić się, że plik został utworzony poprawnie, możemy użyć funkcji "File.exists", która sprawdzi czy plik istnieje. Przykładowy kod prezentuje się tak:

```Elm
File.exists "tmp/plik.docx" --> True 
```

Możemy również użyć funkcji "Basics.toString" aby wyświetlić zawartość pliku w konsoli. Należy jednak pamiętać, że w przypadku dużych plików może to wywołać błąd.

## Deep Dive
Funkcja "File.createTemp" jest bardzo użyteczna, ale istnieje kilka rzeczy, o których warto wiedzieć. Po pierwsze, nazwa pliku musi mieć rozszerzenie, które jest zgodne z typem MIME. Na przykład, jeśli chcemy utworzyć plik tekstowy, nazwa powinna kończyć się na ".txt".

Po drugie, jeśli potrzebujemy utworzyć plik tymczasowy, który zostanie usunięty po zamknięciu programu, możemy użyć funkcji "File.withTemp". Przyjmuje ona funkcję jako argument, która zostanie wykonana na utworzonym pliku.

```Elm
File.withTemp "tmp/" "log.txt" (\path -> 
  Basics.append path "Plik został utworzony tymczasowo."
)
```

W tym przypadku, funkcja "Basics.append" zostanie wykonana na pliku "log.txt" znajdującym się w folderze "tmp". Po zamknięciu programu, plik ten zostanie automatycznie usunięty.

## Zobacz także
- Oficjalna dokumentacja Elm: https://guide.elm-lang.org/
- Operacje na plikach w Elm: https://package.elm-lang.org/packages/elm/file/latest/
- Przykładowe kody w Elm: https://github.com/elm/compiler/tree/master/examples