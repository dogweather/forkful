---
title:                "Elixir: Sprawdzanie istnienia katalogu"
simple_title:         "Sprawdzanie istnienia katalogu"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego?

Sprawdzanie, czy dany katalog istnieje, jest ważnym elementem w wielu projektach Elixir. Powszechnie stosuje się tę metodę w celu wykrycia, czy dany katalog jest dostępny dla odczytu, zapisu lub też aby uniknąć próby skopiowania plików do nieistniejącego katalogu. W tym artykule pokażę Ci, jak łatwo i skutecznie przeprowadzić taką operację w języku Elixir.

## Jak to zrobić?

Sprawdzenie istnienia katalogu można przeprowadzić za pomocą funkcji `File.dir?/1`, gdzie argumentem jest ścieżka do katalogu, którego istnienia chcemy sprawdzić.

```Elixir
# Przykładowa ścieżka do katalogu
path = "/home/user/Downloads"

# Wywołanie funkcji File.dir?()
if File.dir?(path) do
  IO.puts("Podany katalog istnieje!")
else
  IO.puts("Podany katalog nie istnieje.")
end
```

Wywołanie funkcji `File.dir?/1` zwróci wartość `true`, jeśli podany katalog istnieje, lub `false`, jeśli nie istnieje. W przykładowym kodzie powyżej, w zależności od rezultatu, zostanie wyświetlony odpowiedni komunikat.

## Deep Dive

Funkcja `File.dir?/1` wykorzystuje wewnętrznie funkcję `File.stat!/1` do pobrania informacji o danym katalogu. Jeśli zostanie zwrócony błąd `File.StatError`, oznacza to, że podana ścieżka nie jest prawidłowym katalogiem, dlatego funkcja `File.dir?/1` zwraca wartość `false`. W przeciwnym przypadku, jeśli katalog istnieje, funkcja `File.stat!/1` zwróci mapę z informacjami o katalogu, dlatego `File.dir?/1` zwróci wartość `true`.

## Zobacz również

- Dokumentacja Elixir: [File.dir?/1](https://hexdocs.pm/elixir/File.html#dir?/1)
- Funkcje modułu `File` w Elixir: [File](https://hexdocs.pm/elixir/File.html)
- Tutorial Elixir dla początkujących: [Elixir School](https://elixirschool.com/pl)