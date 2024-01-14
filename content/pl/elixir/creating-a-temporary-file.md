---
title:                "Elixir: Tworzenie pliku tymczasowego"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie plików tymczasowych jest powszechnym zadaniem w wielu językach programowania, w tym w Elixirze. Są one przydatne w przypadku, gdy potrzebujemy przechowywać dane na krótki okres czasu i nie chcemy zaśmiecać naszego dysku twardego. Przeczytaj dalszą część tego artykułu, aby dowiedzieć się, jak możesz tworzyć pliki tymczasowe w języku Elixir i wykorzystywać je w swoich projektach.

## Jak to zrobić

Tworzenie plików tymczasowych w Elixirze jest łatwe dzięki wbudowanej bibliotece `File`. Aby utworzyć plik tymczasowy, użyj funkcji `File.temp_file/2` i przekaż jej ścieżkę do katalogu, w którym chcesz utworzyć plik oraz prefiks, który będzie częścią nazwy pliku tymczasowego. Na przykład:

```Elixir
{path, _} = File.temp_file("/tmp", "elixir_")
```

Funkcja zwróci krotkę zawierającą ścieżkę do utworzonego pliku oraz identyfikator pliku. Możesz również określić rozszerzenie pliku tymczasowego, dodając trzeci parametr do funkcji `File.temp_file/3`. Na przykład:

```Elixir
{path, _} = File.temp_file("/tmp", "elixir_", ".txt")
```

Po utworzeniu pliku możesz zapisać do niego dane, korzystając z funkcji `File.write!/2`, na przykład:

```Elixir
File.write!(path, "To jest plik tymczasowy zapisany przez Elixir.")
```

Możesz również odczytać dane z pliku tymczasowego, używając funkcji `File.read!/1`, na przykład:

```Elixir
data = File.read!(path)
```

## Deep Dive

W języku Elixir, pliki tymczasowe są w rzeczywistości plikami o nazwie `.elixir_tempXXX`, gdzie `XXX` to identyfikator pliku. Ten plik jest usuwany automatycznie po zamknięciu procesu lub po skończeniu działania programu. Dzięki temu nie musisz martwić się o usuwanie pliku tymczasowego ręcznie.

W przypadku, gdy chcesz utworzyć plik tymczasowy, który będzie istniał przez dłuższy czas lub potrzebujesz więcej kontroli nad nim, możesz użyć biblioteki `:tempfile`.

## Zobacz też

1. Dokumentacja dla funkcji `File.temp_file/2`: https://hexdocs.pm/elixir/File.html#temp_file/2
2. Dokumentacja dla biblioteki `:tempfile`: https://hexdocs.pm/tempfile/Tempfile.html
3. Przykłady użycia plików tymczasowych w języku Elixir: https://thoughtbot.com/blog/file-system-gotchas-in-elixir