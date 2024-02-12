---
title:                "Tworzenie pliku tymczasowego"
aliases:
- /pl/elixir/creating-a-temporary-file.md
date:                  2024-01-20T17:40:00.515995-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tworzenie pliku tymczasowego"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?
Tworzymy tymczasowy plik, by obrabiać dane, które nie muszą trwać długo. Programiści robią to, by testować kod, przechowywać dane sekwencyjnie, lub ograniczyć zużycie pamięci.

## How to:
W Elixirze nie ma wbudowanej obsługi dla tworzenia tymczasowych plików, ale możemy użyć `System.cmd/3` z `mktemp` dostępnym na większości systemów UNIX.

```elixir
{temp_path, 0} = System.cmd("mktemp", [])
File.write!(temp_path, "Hej, to przykładowa zawartość!")
IO.puts File.read!(temp_path)
File.rm(temp_path)
```
Po uruchomieniu kodu powinniśmy zobaczyć następujące:

```
Hej, to przykładowa zawartość!
```

## Deep Dive
Historia tymczasowych plików zaczyna się od systemów UNIX, które używały ich dla operacji wymagających krótkotrwałego magazynowania danych. Alternatywą w Elixirze może być użycie in-memory storage takiego jak ETS lub wykorzystanie własnej implementacji z użyciem unikalnych nazw. Ważne jest, by pamiętać o odpowiednim usuwaniu tymczasowych plików po użyciu, by nie zostawić "śmieci" na dysku.

## See Also
- [Elixir File module](https://hexdocs.pm/elixir/File.html)
- [ETS - Erlang Term Storage](https://elixir-lang.org/getting-started/mix-otp/ets.html)
- UNIX `mktemp`: man7.org/linux/man-pages/man1/mktemp.1.html
