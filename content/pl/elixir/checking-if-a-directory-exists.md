---
title:                "Sprawdzanie, czy katalog istnieje"
html_title:           "Elixir: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

W programowaniu, sprawdzanie czy dany katalog istnieje to często wykonywane zadanie, polegające na weryfikacji, czy określony katalog istnieje w systemie plików. Robi się to, aby uniknąć błędów podczas próby dostępu do katalogu, który nie istnieje lub został usunięty.

## Jak to zrobić:

W Elixir można sprawdzić, czy katalog istnieje, za pomocą modułu `File`. Poniżej znajduje się przykład:

```Elixir
if File.dir?("/ścieżka/do/katalogu") do
  IO.puts("Katalog istnieje")
else
  IO.puts("Katalog nie istnieje")
end
```
Jeżeli katalog istnieje, program wydrukuje "Katalog istnieje". W przeciwnym razie, zobaczysz "Katalog nie istnieje".

## Szczegółowe omówienie

Historia sprawdzania istnienia katalogu jest tak stara jak historia samych systemów operacyjnych. Ta operacja była dostępna w różnych językach programowania, m.in. w Unix Shell, Pythonie, Javie i wielu innych.

Alternatywą dla `File.dir?/1` jest użycie `File.ls/1`, które zwraca listę plików i katalogów znajdujących się w danym katalogu. Jeżeli katalog nie istnieje, `File.ls/1` zwróci błąd, co pozwala na dodatkowe kontrolowanie zachowania kodu.

Implementacja sprawdzania istnienia katalogu w Elixir jest zrealizowana za pomocą wbudowanego modułu `File`, który komunikuje się z systemem operacyjnym za pomocą natywnych funkcji języka Erlang.

## Zobacz także:

W celu dalszego zgłębiania wiedzy na temat pracy z katalogami i plikami w Elixir, odwiedź następujące zasoby:

1. [Dokumentacja modułu File](https://hexdocs.pm/elixir/File.html)
2. [Praca z plikami i katalogami w Elixir](https://elixir-lang.org/getting-started/io-and-the-file-system.html)