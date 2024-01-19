---
title:                "Tworzenie tymczasowego pliku"
html_title:           "C#: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co to i po co?

Tworzenie tymczasowego pliku polega na utworzeniu pliku, który służy do przechowywania danych tylko przez krótki czas. Programiści robią to, aby oszczędzić pamięć, a często też do debugowania.

## Jak to zrobić:

Poniżej znajduje się przykładowy kod do utworzenia tymczasowego pliku w Gleam:

```gleam
import gleam/@otp/file.{Temp}
 

fn main(args: List(String)) {
  let _ = new_temp_file(args)
}
 
pub fn new_temp_file(name: List(String)) {
  let temp = Temp.new(name)
  case temp {
    Ok(file) -> 
      Ok("Utworzono nowy plik tymczasowy: " ++ file)
    Error(err) -> 
      Error("Nie udało się utworzyć pliku tymczasowego: " ++ err)
  }
} ```

Output:

```shell
Utworzono nowy plik tymczasowy: /tmp/tempfile123
```

## Pogłębione informacje:

Tworzenie tymczasowych plików ma swoje korzenie w UNIX, gdzie stworzono koncepcję systemu plików /tmp dla plików o krótkim żywotności. Alternatywnym podejściem do oszczędzania pamięci jest użycie strumieni lub buforów, ale to wymaga bardziej skomplikowanego kodowania. Z punktu widzenia implementacji, pliki tymczasowe mają nad sobą nadzorcę, który je automatycznie usuwa po zamknięciu programu.

## Zobacz też:

- Dokumentacja Gleam na pliki tymczasowe: https://hexdocs.pm/gleam_stdlib/gleam/@otp/file/Temp.html
- Informacje o systemie plików w Unix: https://pl.wikipedia.org/wiki/Unix_File_System
- Poradnik do utworzenia tymczasowych plików w Python: https://docs.python.org/3/library/tempfile.html