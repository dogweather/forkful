---
title:                "Gleam: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Stwarzanie tymczasowego pliku jest bardzo przydatne w wielu projektach programistycznych, ponieważ pozwala na tymczasowe przechowywanie danych, manipulowanie nimi oraz łatwe usuwanie ich po zakończeniu pracy.

## Jak to zrobić

Aby stworzyć tymczasowy plik w języku programowania Gleam, należy użyć modułu `gleam/file` oraz funkcji `File.temporary`. Poniżej znajduje się przykładowy kod dla systemu UNIX:

```Gleam
use gleam/file
import gleam/file.TemporaryFile
import gleam/file.WriteMode

result =
    File.temporary("my_temp_file", extension = "txt", write_mode = WriteMode.ReadWrite) // Tworzy tymczasowy plik o nazwie "my_temp_file.txt"
    |> TemporaryFile.write("Lorem ipsum") // Zapisuje tekst do pliku
    |> TemporaryFile.read() // Wczytuje zawartość pliku i zwraca ją jako wynik

// Wynik: Ok("Lorem ipsum") 
```

## Głębsza analiza

Moduł `gleam/file` udostępnia także inne przydatne funkcje związane z tworzeniem tymczasowych plików, takie jak `TemporaryFile.delete()` czy `TemporaryFile.rename()`. Warto również pamiętać, że plik tymczasowy zostanie automatycznie usunięty po zakończeniu działania programu, więc nie musimy martwić się o jego ręczne usuwanie.

## Zobacz również

- Dokumentacja modułu `gleam/file`: https://gleam.run/modules/gleam_file/
- Przewodnik po języku Gleam: https://gleam.run/book/
- Inne artykuły na temat programowania w języku Gleam: https://gleam.run/articles/