---
title:                "Elixir: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego
Ciągłe łączenie ciągów znaków jest nieodzownym elementem wielu programów Elixir. Pozwala to na tworzenie dynamicznych tekstów, które mogą zmieniać się w zależności od różnych warunków i zmiennych. Jest to również wygodny sposób na tworzenie interfejsów użytkownika czy generowanie raportów. W tym artykule dowiecie się, dlaczego jest to ważne i jak tego dokonać.

## Jak to zrobić
Aby połączyć dwa ciągi znaków w Elixir, możemy skorzystać z funkcji `<>`. W poniższym przykładzie używamy go do połączenia imienia i nazwiska w jedną zmienną:

```Elixir
first_name = "Anna"
last_name = "Kowalska"

full_name = first_name <> " " <> last_name

IO.puts(full_name)
# Wynik: "Anna Kowalska"
```

Jak widać, użycie `<>` pozwala nam na połączenie kilku ciągów znaków w jedną zmienną. Możemy także połączyć więcej niż dwa ciągi znaków jednocześnie.

Aby dokonać konkatenacji większej liczby ciągów, możemy skorzystać z funkcji `Enum.reduce/3`. Dzięki niej możemy iteracyjnie łączyć elementy listy w jedną zmienną, co jest przydatne np. w przypadku tworzenia listy zakupów czy wierszy danych do pliku CSV:

```Elixir
items = ["jajka", "mleko", "chleb", "masło"]

shopping_list = Enum.reduce(items, "", fn item, result ->
  result <> item <> ", "
end)

IO.puts(shopping_list)
# Wynik: "jajka, mleko, chleb, masło, "
```

## Deep Dive
Funkcja `<>` w rzeczywistości jest odmianą funkcji `Kernel.<>/2`, która jest częścią języka Elixir. Pozwala ona na łączenie różnych typów danych, np. liczb z ciągami znaków. Jednak zaleca się unikanie takich operacji, ponieważ może to prowadzić do błędów i niepożądanych zachowań.

Funkcja `Enum.reduce/3` jest często używana do łączenia większej liczby ciągów, ponieważ pozwala na iteracyjne łączenie, co jest wygodniejsze i bardziej wydajne. Jednak może również być używana do innych operacji na listach, takich jak sumowanie wartości czy filtrowanie elementów.

## Zobacz także
- [Dokumentacja funkcji Kernel.<>/2](https://hexdocs.pm/elixir/Kernel.html#/2)
- [Dokumentacja funkcji Enum.reduce/3](https://hexdocs.pm/elixir/Enum.html#reduce/3)
- [Artykuł na temat łączenia ciągów znaków w Elixirze](https://andrewtimberlake.com/blog/elixir-string-concatenation)