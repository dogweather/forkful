---
title:                "Gleam: Tworzenie pliku tymczasowego"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Stworzenie tymczasowego pliku może być niezbędne w wielu różnych sytuacjach. Na przykład, jeśli musimy wykonać pewne operacje na danych tymczasowych, ale nie chcemy lub nie możemy zmieniać istniejącego pliku, to tworzenie nowego, tymczasowego pliku może być bardzo przydatne.

## Jak to zrobić

Możemy wykorzystać funkcję `open_temporary_file` z biblioteki standardowej Gleam, aby utworzyć tymczasowy plik. Funkcja ta przyjmuje jako argument nazwę pliku oraz tryb, w jakim ma zostać otwarty. Następnie możemy wykonać operacje na tym pliku, a po zakończeniu działania plik zostanie automatycznie usunięty.

```Gleam
import standard/file

let file = open_temporary_file("temporary.txt", :write)
file.write("To jest przykładowy tekst")
file.close()
```

W powyższym przykładzie tworzymy tymczasowy plik o nazwie "temporary.txt" i pozwalamy sobie na jego edycję, wybierając tryb `:write`. Następnie za pomocą funkcji `write` zapisujemy do pliku nasz przykładowy tekst, a na koniec zamykamy plik. Po wykonaniu powyższych operacji plik zostanie automatycznie usunięty.

## Pogłębione wyjaśnienia

Tworząc tymczasowy plik przy użyciu funkcji `open_temporary_file`, możemy wybrać spośród kilku trybów otwarcia. Możemy na przykład tylko odczytywać plik (`:read`) lub tylko zapisywać (`:write`). W przypadku, gdy chcemy mieć możliwość jednoczesnego czytania i zapisywania, możemy wybrać tryb `:read_write`.

Ponadto, funkcja `open_temporary_file` zwraca złożony typ danych `Result`, który może zawierać wartość pliku lub błąd, jeśli utworzenie pliku nie powiodło się. Dzięki temu możemy obsłużyć ewentualne błędy w naszym kodzie.

## Zobacz także

- Dokumentacja biblioteki standardowej Gleam dotycząca funkcji `open_temporary_file`
- Przykładowy kod korzystający z funkcji `open_temporary_file`
- Inne przydatne funkcje z biblioteki standardowej Gleam