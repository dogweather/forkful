---
title:                "Elixir: Łączenie ciągów znaków"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu Elixirem będziesz potrzebował połączyć ciągi znaków w jedną zmienną. Może to być potrzebne do wyświetlenia wiadomości na ekranie lub tworzenia adresów URL. W artykule tym pokażę, jak prosto można to zrobić w Elixirze.

## Jak To Zrobić

W Elixirze do konkatenacji stringów mamy dwa sposoby - operator `<>` i funkcję `String.concat/2`. Zobaczmy najpierw przykład z użyciem operatora:

```Elixir
string1 = "Hello"
string2 = "world!"

concatenated_string = string1 <> " " <> string2
```

W rezultacie zmienna `concatenated_string` będzie zawierać tekst "Hello world!".

Teraz przejdźmy do przykładu z użyciem funkcji `String.concat/2`:

```Elixir
string1 = "Witaj"
string2 = "świecie!"

concatenated_string = String.concat([string1, " ", string2])
```

W obu przypadkach używamy znaku spacji jako separatora, ale może to być dowolny znak lub nawet pusty string.

## Głębsze Zanurzenie

W Elixirze konkatenacja stringów jest wydajniejsza niż w innych językach programowania, ponieważ jest wykonywana w czasie kompilacji, a nie wykonania. Oznacza to, że nie trzeba się martwić o wydajność, gdy używamy konkatenacji w pętlach lub w innych miejscach, gdzie wykonuje się wiele operacji na stringach.

Warto również wiedzieć, że operator `<>` jest przeciążony dla większości typów danych, więc można go stosować nie tylko do stringów, ale także np. do list czy map.

## Zobacz Również

- Dokumentacja operatora `<>`: https://hexdocs.pm/elixir/operators.html#concatenation-operator-
- Dokumentacja funkcji `String.concat/2`: https://hexdocs.pm/elixir/String.html#concat/2
- Blog o Elixirze po polsku: https://elixir.pl/