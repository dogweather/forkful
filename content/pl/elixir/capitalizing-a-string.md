---
title:                "Pisownia z wielkiej litery ciągu znaków"
html_title:           "Elixir: Pisownia z wielkiej litery ciągu znaków"
simple_title:         "Pisownia z wielkiej litery ciągu znaków"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, dlaczego otrzymując tekst od użytkownika, nazywającego się na przykład "anna", chcesz go zamienić na "Anna"? Czy nie byłoby łatwiej zaoszczędzić sobie czasu i pracy, pozwalając tej osobie na użycie ulubionej wielkości liter w swoim imieniu? Jednak z punktu widzenia estetyki i przyjętych konwencji, ważne jest, aby tekst był poprawnie napisany z punktu widzenia wielkości liter. W Elixirze istnieje narzędzie, które może ci w tym pomóc - funkcja `String.capitalize/1`.

## Jak to zrobić

W celu użycia funkcji `String.capitalize/1` należy najpierw wprowadzić do swojego kodu moduł `String`, używając polecenia `require String`. Następnie wywołujemy funkcję, podając jako argument nasz tekst, który chcemy zamienić na poprawną wielkość liter. Na przykład:

```Elixir
require String

String.capitalize("anna")
# Wynik: "Anna"
```

Funkcja `String.capitalize/1` jest również w stanie zamienić wielkość liter dla całego tekstu, a nie tylko pierwszej litery. W takim przypadku, dla tekstu "jeśli zdroweś to złóż żądanie", wynikiem będzie "Jeśli Zdroweś To Złóż Żądanie". Można to zrobić, dodając opcję `:all` do funkcji:

```Elixir
String.capitalize("if you're happy and you know it", :all)
# Wynik: "If You're Happy And You Know It"
```

Możliwe jest także użycie funkcji `String.capitalize/1` dla wielu wyrazów jednocześnie, wykorzystując pętlę `Enum.map`:

```Elixir
texts = ["anna", "emma", "tomasz"]

Enum.map(texts, &String.capitalize/1)
# Wynik: ["Anna", "Emma", "Tomasz"]
```

## Głębsze wewnętrzne działanie

Podczas gdy funkcja `String.capitalize/1` jest prosta w użyciu, warto wiedzieć, jak działa wewnętrznie. Otóż, funkcja ta wykorzystuje moduł `String.Special`, który zawiera listę wyjątków oraz reguły do zamiany liter w różnych językach. Dzięki temu, funkcja potrafi obsłużyć specjalne przypadki, takie jak zamiana litery "i" na "I" w jednym słowie, ale nie w drugim.

## Zobacz także

- [Dokumentacja funkcji String.capitalize/1](https://hexdocs.pm/elixir/String.html#capitalize/2)
- [Dokumentacja modułu String.Special](https://hexdocs.pm/elixir/String.Special.html)
- [Przewodnik po Elixirze](https://elixir-lang.org/getting-started/introduction.html)