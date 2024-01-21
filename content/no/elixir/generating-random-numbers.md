---
title:                "Generering av tilfeldige tall"
date:                  2024-01-20T17:49:04.499249-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generering av tilfeldige tall"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generering av tilfeldige tall brukes for å skape unike data eller simulere hendelser. Programmerere trenger dette for tester, spilllogikk, sikkerhet og mer.

## How to:
I Elixir kan du generere tilfeldige tall ved å bruke `rand` modulen fra Erlangs `:rand` bibliotek:

```elixir
# Generer et tilfeldig tall mellom 0 og 1
random_float = :rand.uniform()
IO.puts(random_float)

# Generer et tilfeldig heltall mellom 1 og 10
random_integer = :rand.uniform(10)
IO.puts(random_integer)
```

Eksempelutdata kan se slik ut:

```plaintext
0.443585
7
```

For å sikre at tallene virkelig er tilfeldige ved hver kjøring, initialiser :rand med en annen seed:

```elixir
:rand.seed(:exs1024, :os.timestamp())
```

## Deep Dive
`rand` modulen bruker algoritmer for pseudotilfeldige tallgeneratorer, som ikke er 100% tilfeldige, men gode nok for mange brukstilfeller. Elixir's tilfeldighet er velegnet for modellering og simulering, men anbefales ikke for kryptografiske formål. I historisk sammenheng har språk som Elixir lånt mye fra Erlang, både i bruksmønstre og biblioteker, inkludert tilfeldighetshåndteringen. 

For sikkerhetskritiske anvendelser, vurder å bruke kryptografisk sikre biblioteker som `:crypto.strong_rand_bytes/1` for tilfeldige tall som er vanskeligere å forutsi.

## See Also
- [Erlang :rand Module Documentation](http://www.erlang.org/doc/man/rand.html)
- [Elixir School - Random](https://elixirschool.com/en/lessons/basics/collections/#tuples)