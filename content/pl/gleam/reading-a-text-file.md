---
title:                "Odczytywanie pliku tekstowego"
html_title:           "Gleam: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego warto przeczytać plik tekstowy? Oczywiście, czasem potrzebujemy uzyskać informacje z tego pliku, jednak w Gleam możemy to zrobić w łatwy i efektywny sposób, co dowiemy się poniżej.

## Jak to zrobić

```Gleam
import gleam/file

let file = file.read("file.txt")

file
|> case
    Ok(contents) -> contents  // Jeśli udało się odczytać plik, zwróć jego zawartość
    Err(_) -> "Błąd podczas odczytu pliku"  // W przypadku błędu, zwróć informację o błędzie
```

Używając modułu `gleam/file`, możemy wykorzystać funkcję `read`, aby wczytać zawartość pliku tekstowego. Następnie możemy użyć konstrukcji `case` do sprawdzenia, czy operacja się powiodła, i zwrócić odpowiedni wynik.

Output dla pliku `file.txt` z zawartością `Hello World!`:

```
Ok("Hello World!")
```

Deep Dive: W przypadku plików z rozszerzeniem `.txt`, funkcja `read` zwraca typ `Result(Ok(String), Error)`, co oznacza, że możemy otrzymać albo wartość typu `Ok` z zawartością pliku, albo wartość typu `Err` z odpowiednim błędem. W przypadku innych typów plików, funkcja `read` może zwrócić typ `Result(Ok(Binary), Error)`, gdzie wartość typu `Ok` zawiera binarną reprezentację pliku.

## Zobacz również

- Dokumentacja modułu `gleam/file`: https://gleam.run/docs/std/file
- Przykłady użycia funkcji `read`: https://github.com/gleam-lang/gleam/blob/main/examples/file_reader.gleam