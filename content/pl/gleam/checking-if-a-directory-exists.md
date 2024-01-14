---
title:                "Gleam: Sprawdzanie czy istnieje katalog"
simple_title:         "Sprawdzanie czy istnieje katalog"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzenie, czy katalog istnieje, jest ważnym aspektem programowania w Gleam. Dzięki temu można uniknąć błędów i zapewnić poprawne działanie programu. 

## Jak to zrobić

Sprawdzenie istnienia katalogu jest proste w Gleam. Wystarczy użyć funkcji `exists` z modułu `gleam/io/fs`. Przykładowy kod wyglądałby następująco: 

```Gleam
import gleam/io/fs

fn main() {
  let directory = "images"
  let exists = gleam/io/fs.exists(directory)
  case exists {
    Ok(_) -> 
        io.print("Katalog istnieje!")
    Err(_) -> 
        io.print("Katalog nie istnieje")
  }
}
``` 

W powyższym przykładzie, najpierw importujemy moduł `gleam/io/fs`, a następnie tworzymy zmienną `directory`, która przechowuje nazwę sprawdzanego katalogu. Następnie używamy funkcji `exists`, która zwraca wartość typu `Result` - `Ok` jeśli katalog istnieje, lub `Err` jeśli go nie ma. W zależności od tego, jaką wartość zwróci funkcja, wyświetlamy odpowiedni komunikat. W ten sposób możemy w łatwy sposób sprawdzić, czy dany katalog istnieje.

## Deep Dive

Funkcja `exists` znajduje się w module `gleam/io/fs`, który zawiera wiele innych przydatnych funkcji do operacji na plikach i katalogach. W przypadku, gdy chcemy sprawdzić, czy katalog istnieje oraz czy jest on pusty, możemy skorzystać z funkcji `is_empty`, która również zwraca wartość typu `Result`. 

Moduł `gleam/io/fs` zawiera również inne przydatne funkcje, takie jak `read`, `write`, czy `rename`, które pozwalają na czytanie, zapisywanie i zmianę nazwy plików. Dzięki temu, programowanie w Gleam staje się prostsze i bardziej wydajne.

## Zobacz również

- Dokumentacja modułu `gleam/io/fs`: https://gleam.run/modules/io-fs.html
- Oficjalna strona języka Gleam: https://gleam.run/
- Przykładowe projekty i biblioteki w Gleam: https://gleam.run/community.html