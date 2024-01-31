---
title:                "Korzystanie z interaktywnego shella (REPL)"
date:                  2024-01-26T04:13:36.307589-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z interaktywnego shella (REPL)"

category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Interaktywna powłoka, czyli REPL (Read-Eval-Print Loop - Pętla Czytaj-Ewaluuj-Wydrukuj), pozwala na testowanie fragmentów kodu w czasie rzeczywistym. Programiści Elixira używają REPL, zwanego IEx (Interaktywny Elixir), do eksperymentowania, debugowania i uczenia się języka.

## Jak to zrobić:
Aby uruchomić IEx, otwórz terminal i wpisz `iex`. Oto mały przedsmak:

```Elixir
iex> name = "Programista Elixir"
"Programista Elixir"
iex> String.length(name)
17
iex> Enum.map([1, 2, 3], fn num -> num * 3 end)
[3, 6, 9]
```

Wynik powinien pokazać przypisanie zmiennej, wyniki funkcji oraz anonimową funkcję w działaniu.

## Dogłębna analiza
Powłoka IEx jest częścią Elixira od jego wczesnych dni. José Valim, twórca Elixira, zainspirował się interaktywnymi powłokami innych języków, takich jak `python` w Pythonie i `irb` w Ruby. Chociaż IEx dzieli wiele funkcji z nimi, zostało zbudowane tak, aby radzić sobie ze współbieżnością charakterystyczną dla Elixira i jest w pełni zintegrowane z możliwościami maszyny wirtualnej Erlanga (Erlang VM).

Alternatywy dla IEx w ekosystemie Erlanga obejmują `erl`, powłokę Erlanga. Jednak IEx zapewnia bardziej przyjazne środowisko dla Elixira, z funkcjami takimi jak obszerne uzupełnianie tabulacyjne, historia i pomocnicy.

REPL IEx to coś więcej niż plac zabaw; może bezproblemowo łączyć się z działającym systemem. Jest to kluczowe dla debugowania aplikacji na żywo. Podstawowa implementacja opiera się na BEAM (maszyna wirtualna Erlanga), zapewniając wsparcie funkcji takich jak gorąca wymiana kodu bezpośrednio w powłoce.

## Zobacz również
Sprawdź to, aby uzyskać więcej informacji i zasobów:

- [Dokumentacja IEx w Elixirze](https://hexdocs.pm/iex/IEx.html)
- [Interaktywny Elixir (IEx) - Powłoka Elixira](https://elixir-lang.org/getting-started/introduction.html#interactive-elixir)
- [Dokumentacja `erl` w Erlangu](http://erlang.org/doc/man/erl.html)
- [Nauka interaktywnej powłoki Elixira](https://elixirschool.com/en/lessons/basics/iex_helpers/)
