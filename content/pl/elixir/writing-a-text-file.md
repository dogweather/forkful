---
title:                "Zapisywanie pliku tekstowego"
html_title:           "Arduino: Zapisywanie pliku tekstowego"
simple_title:         "Zapisywanie pliku tekstowego"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego?

Pisanie do pliku tekstowego to zapisywanie danych w formie czytelnej dla człowieka na dysku. Programiści robią to, by trwale zachować wyniki, konfiguracje czy logi.

## Jak to zrobić:

```elixir
# Otworzenie (lub utworzenie) pliku do zapisu:
File.write!("hello.txt", "Witaj, Elixir!")

# Sprawdzenie zawartości pliku:
File.read!("hello.txt")
```

Wynik:

```elixir
"Witaj, Elixir!"
```

## W głębi tematu:

Pisanie do pliku w Elixirze opiera się na modułach BEAM (Erlang VM), zapewniających efektywność i skalowalność. Alternatywą jest użycie funkcji `Stream`, która pozwala na operacje na plikach w sposób leniwy (lazy evaluation). Szczegóły implementacji w Elixirze ułatwiają obłsugę błędów i zapewniają czysty, funkcjonalny styl kodu.

## Zobacz też:

- [Elixir Documentation on File Module](https://hexdocs.pm/elixir/File.html)
- [Erlang's :file module used by Elixir](http://erlang.org/doc/man/file.html)
- [Learn Elixir - Working with Files (tutorial)](https://elixirschool.com/en/lessons/basics/collections/)