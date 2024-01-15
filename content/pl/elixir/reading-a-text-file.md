---
title:                "Odczytywanie pliku tekstowego"
html_title:           "Elixir: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek chciałeś/-aś przeczytać zawartość pliku tekstowego w swoim kodzie Elixir? Może potrzebowałeś/-aś pobrać dane ze zewnętrznego źródła, a plik tekstowy był jedynym dostępnym sposobem. W tym artykule dowiesz się, jak w łatwy sposób odczytać plik tekstowy w Elixir i wykorzystać jego zawartość w swoim kodzie.

## Jak to zrobić

Pierwszym krokiem jest otwarcie pliku tekstowego za pomocą funkcji `File.open/2`, gdzie pierwszym argumentem jest ścieżka do pliku, a drugim tryb dostępu. Następnie możesz odczytać zawartość pliku za pomocą funkcji `IO.read/2`, gdzie pierwszym argumentem jest otwarty plik, a drugim liczba bajtów do odczytania. Na przykład:

```elixir
file = File.open("plik.txt", [:read, :utf8])
contents = IO.read(file, 10)
IO.puts contents
```

W powyższym przykładzie otwieramy plik "plik.txt" w trybie odczytu i ustawiamy kodowanie na UTF-8. Następnie odczytujemy 10 bajtów z pliku i wypisujemy je na ekran. 

Możesz również przeczytać całą zawartość pliku jednym poleceniem za pomocą funkcji `File.read/1`:

```elixir
contents = File.read("plik.txt")
IO.puts contents
```

Oczywiście, jeśli nie chcesz czytać całego pliku, możesz również wykorzystać funkcję `IO.read/1` zamiast `File.read/1` i przekazać jej liczbę bajtów do odczytania. 

## Deep Dive

Podczas odczytywania pliku tekstowego w Elixir, warto pamiętać o kilku ważnych rzeczach. Po pierwsze, upewnij się, że kodowanie pliku jest zgodne z tym, który ustawiłeś lub domyślnym dla Twojego systemu. Możesz to zrobić, ustawiając drugi argument w funkcjach `File.open/2` i `File.read/2`.

Kolejną rzeczą do zapamiętania jest sposób w jaki traktowany jest ostatni znak nowej linii w pliku. W Elixir, znak nowej linii jest traktowany jako `'\n'`, jednak niektóre systemy operacyjne mogą używać innego znaku, takiego jak `'\r\n'`. Dlatego warto użyć funkcji `String.trim_trailing/1` lub `String.trim_trailing/2` aby pozbyć się tych znaków nowej linii w odczytanym tekście.

## Zobacz także

- [Dokumentacja Elixir na temat plików](https://hexdocs.pm/elixir/File.html)
- [Poradnik programowania w Elixir](https://elixirschool.com/pl/)
- [Pierwsze kroki z Elixir](https://elixir-lang.org/getting-started/introduction.html)