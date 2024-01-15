---
title:                "Usuwanie znaków dopasowujących się do wzorca"
html_title:           "Elixir: Usuwanie znaków dopasowujących się do wzorca"
simple_title:         "Usuwanie znaków dopasowujących się do wzorca"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Usuwanie znaków pasujących do wzoru może być przydatne w wielu przypadkach, od transformacji danych po oczyszczanie tekstu czy obliczanie statystyk. W Elixirze istnieją różne funkcje, które pozwalają nam na wykonanie tego zadania w łatwy i efektywny sposób.

## Jak to zrobić

Możemy użyć funkcji `String.replace/3` do zastąpienia znaków pasujących do wzoru pustym ciągiem, co w efekcie spowoduje ich usunięcie. Przykładowo:

```Elixir
input = "Hello Elixir World"
String.replace(input, ~r/Elixir/, "")
# Output: "Hello World"
```

Możemy także wykorzystać funkcję `String.replace/4`, która w połączeniu z flagą `global: true` umożliwia usunięcie wszystkich pasujących znaków z danego ciągu. Przykładowo:

```Elixir
input = "abc123def456ghi"
String.replace(input, ~r/[0-9]+/, "", global: true)
# Output: "abcdefghi"
```

Możemy również skorzystać z biblioteki `Regex` i funkcji `Regex.replace/3`, która pozwala nam na bardziej zaawansowane operacje usuwania znaków pasujących do wzoru. Przykładowo:

```Elixir
input = "abc123def456ghi"
Regex.replace(~r/[0-9]+/, input, "", global: true)
# Output: "abcdefghi"
```

## Wnikliwsze spojrzenie

Podczas usuwania znaków zgodnych z wzorem, warto zwrócić uwagę na wybór odpowiedniej funkcji oraz flag, które umożliwią nam wykonywanie konkretnych operacji. W Elixirze istnieje również wiele innych funkcji i bibliotek, które mogą być przydatne podczas usuwania znaków pasujących do wzoru, takich jak `String.trim/2`, `String.split/3` czy `String.strip/2`.

## Zobacz także

- Oficjalna dokumentacja Elixir: https://hexdocs.pm/elixir/
- Dokumentacja funkcji `String.replace/3`: https://hexdocs.pm/elixir/String.html#replace/3
- Dokumentacja funkcji `Regex.replace/3`: https://hexdocs.pm/elixir/Regex.html#replace/3