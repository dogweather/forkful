---
title:    "Elixir: Tworzenie tymczasowego pliku."
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie tymczasowego pliku może być niezbędne w wielu przypadkach programowania w Elixir. Może być to przydatne do przechowywania danych tymczasowych lub do testowania kodu. Jednak niektórzy programiści mogą nie być pewni, jak stworzyć i korzystać z tymczasowych plików w Elixir. W tym artykule dowiesz się jak to zrobić.

## Jak to zrobić

Jedną z najprostszych metod tworzenia tymczasowego pliku jest użycie funkcji `Path.join/2` i `File.write!/2`. Sprawdźmy to na przykładzie:

```Elixir
file_name = Path.join(["/tmp", "example.txt"])
content = "To jest przykładowy tekst który zostanie zapisany w pliku."
File.write!(file_name, content)
```

W powyższym kodzie tworzymy zmienną `file_name`, która zawiera ścieżkę do pliku "/tmp/example.txt". Następnie tworzymy zmienną `content`, która zawiera tekst, który chcemy zapisać w pliku. W ostatnim kroku używamy funkcji `write!/2` aby zapisać zawartość zmiennej `content` do pliku o nazwie podanej w zmiennej `file_name`.

Możesz również wykorzystać funkcję `Tempfile.open/2` aby stworzyć tymczasowy plik w wybranym przez Ciebie miejscu. Wykorzystajmy to w innym przykładzie:

```Elixir
file = Tempfile.open("example.txt", "/tmp")
IO.puts(file.path)
```

W powyższym kodzie, funkcja `Tempfile.open/2` tworzy tymczasowy plik o nazwie "example.txt" w folderze "/tmp". Następnie używamy funkcji `IO.puts/2` aby wyświetlić ścieżkę do tego pliku w konsoli.

## Ciekawostki

W przypadku gdy chcesz stworzyć tymczasowy plik w obecnym folderze, użyj funkcji `Tempfile.tmpfile/0`. Pamiętaj również, że plik zostanie automatycznie usunięty po tym, jak zostanie zamknięty.

## Zobacz również

- Dokumentacja Elixir - [Tworzenie tymczasowych plików](https://hexdocs.pm/elixir/File.html#write!/2)
- Blog o Elixir - [Przetwarzanie plików tymczasowych w Elixir](https://dev.to/craftninja/processing-temporary-files-in-elixir-2ke6)