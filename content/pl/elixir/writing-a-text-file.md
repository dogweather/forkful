---
title:                "Pisanie pliku tekstowego"
html_title:           "Elixir: Pisanie pliku tekstowego"
simple_title:         "Pisanie pliku tekstowego"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie plików tekstowych jest niezbędnym elementem programowania w języku Elixir. Pozwala ono na przechowywanie i przetwarzanie danych w prosty i łatwy sposób. Dzięki temu artykułowi dowiesz się, jak w prosty sposób pisać pliki tekstowe w Elixirze.

## Jak to zrobić

Pierwszym krokiem jest otworzenie pliku w trybie "write". Następnie, użyj funkcji "IO.puts" w celu zapisania danych do pliku. Na przykład:

```
Elixir
file = File.open("plik.txt", [:write])
IO.puts(file, "To jest przykładowy tekst")
File.close(file)
```

W powyższym przykładzie, otwieramy plik o nazwie "plik.txt" w trybie zapisu. Następnie, używamy funkcji "IO.puts" aby zapisać tekst do pliku. Na koniec, zamykamy plik przy użyciu funkcji "File.close". Możesz też użyć funkcji "IO.write" aby zapisać dane w inny sposób.

## Pogląd w głębi

Pisanie plików tekstowych w Elixirze jest bardzo proste i nie wymaga skomplikowanych działań. Możesz również użyć funkcji "IO.binwrite" aby zapisać dane do pliku w postaci binarnej. Warto pamiętać, że funkcje "IO.puts" oraz "IO.write" mogą być użyte wielokrotnie w celu zapisania kolejnych linii tekstu do pliku.

## Zobacz także

- Dokumentacja Elixir: https://elixir-lang.org/docs.html
- Github Elixir: https://github.com/elixir-lang/elixir
- Tutorial Elixir: https://elixir-lang.org/getting-started/introduction.html