---
date: 2024-01-20 17:40:00.515995-07:00
description: "How to: W Elixirze nie ma wbudowanej obs\u0142ugi dla tworzenia tymczasowych\
  \ plik\xF3w, ale mo\u017Cemy u\u017Cy\u0107 `System.cmd/3` z `mktemp` dost\u0119\
  pnym na wi\u0119kszo\u015Bci system\xF3w\u2026"
lastmod: '2024-03-13T22:44:35.064193-06:00'
model: gpt-4-1106-preview
summary: "W Elixirze nie ma wbudowanej obs\u0142ugi dla tworzenia tymczasowych plik\xF3\
  w, ale mo\u017Cemy u\u017Cy\u0107 `System.cmd/3` z `mktemp` dost\u0119pnym na wi\u0119\
  kszo\u015Bci system\xF3w UNIX."
title: Tworzenie pliku tymczasowego
weight: 21
---

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
