---
date: 2024-01-20 17:40:00.515995-07:00
description: "Tworzymy tymczasowy plik, by obrabia\u0107 dane, kt\xF3re nie musz\u0105\
  \ trwa\u0107 d\u0142ugo. Programi\u015Bci robi\u0105 to, by testowa\u0107 kod, przechowywa\u0107\
  \ dane sekwencyjnie, lub\u2026"
lastmod: 2024-02-19 22:04:54.241875
model: gpt-4-1106-preview
summary: "Tworzymy tymczasowy plik, by obrabia\u0107 dane, kt\xF3re nie musz\u0105\
  \ trwa\u0107 d\u0142ugo. Programi\u015Bci robi\u0105 to, by testowa\u0107 kod, przechowywa\u0107\
  \ dane sekwencyjnie, lub\u2026"
title: Tworzenie pliku tymczasowego
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
