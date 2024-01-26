---
title:                "Sprawdzanie, czy katalog istnieje"
date:                  2024-01-20T14:56:26.626273-07:00
html_title:           "Fish Shell: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Sprawdzanie, czy katalog istnieje, to prosty test, który mówi nam, czy ścieżka w systemie plików prowadzi do katalogu. Programiści robią to, aby uniknąć błędów przy próbie dostępu do katalogu, który nie istnieje, lub aby utworzyć katalog, jeśli jeszcze go nie ma.

## Jak to zrobić:
Aktualna wersja Gleam nie posiada wbudowanej funkcji do bezpośredniego sprawdzania, czy katalog istnieje, więc musisz użyć zewnętrznych paczek lub rozwiązań systemowych.

```gleam
// Załóż, że korzystasz z hipotetycznej biblioteki 'filesystem'

import filesystem

pub fn check_directory_exists(path: String) -> Bool {
  filesystem.dir_exists(path)
}

// Przy użyciu funkcji w praktyce
pub fn main() {
  let path = "/path/to/directory"
  
  let exists = check_directory_exists(path)
  case exists {
    True -> io.println("Katalog istnieje")
    False -> io.println("Katalog nie istnieje")
  }
}
```

### Wyjście przykładowe
```
Katalog istnieje
```
Albo, jeśli katalog nie istnieje:
```
Katalog nie istnieje
```

## Pogłębienie wiedzy
Sprawdzanie istnienia katalogu jest podstawową operacją w wielu językach programowania. Tradycyjnie, dobrze zaprojektowane aplikacje unikały zakładania, że zasoby na dysku istnieją, i zamiast tego dynamicznie reagowały na ich dostępność. Mimo braku natywnej obsługi tej funkcjonalności w Gleam można skorzystać z zewnętrznych bibliotek, jak `filesystem` w przykładzie, lub użyć interfejsów FFI (Foreign Function Interface) do wywołania odpowiednich funkcji z języków pozwalających na łatwe sprawdzanie istnienia katalogów, na przykład Rust czy C. Alternatywnie, jeśli operujesz w środowisku Unixowym, możesz po prostu skorzystać z wywołania systemowego za pomocą `os.cmd` do `ls` lub `test -d`, choć wiązać się to będzie z pewną nieporęcznością i dodatkową obsługą błędów.

## Zobacz również
- [Erlang :file module](http://erlang.org/doc/man/file.html) - na wypadek gdybyś chciał stworzyć własną abstrakcję wykorzystując Erlanga
- [Bash 'test' command](https://man7.org/linux/man-pages/man1/test.1.html) - dla przykładów użycia poleceń systemowych w Unix/Linux
