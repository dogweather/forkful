---
title:    "Elixir: Wyciąganie podciągów"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Dlaczego?

W dzisiejszych czasach, wiele aplikacji i stron internetowych wymaga działania na napisach lub znakach. Jednym ze sposobów manipulacji napisami jest wyodrębnienie podciągów, czyli fragmentów tekstu. W tym artykule dowiecie się, dlaczego warto poznać podstawy wyodrębniania podciągów w języku Elixir.

## Jak to zrobić?

W języku Elixir, do wyodrębniania podciągów można użyć funkcji `String.slice/3`. Przyjmuje ona trzy argumenty: napis, początkowy indeks oraz końcowy indeks. Dzięki temu możemy w łatwy sposób wyciągnąć interesujące nas fragmenty tekstu.

```Elixir
string = "Witaj, Polsko!"

# Wyodrębnienie podciągu zawierającego słowo "Polsko"
String.slice(string, 7, 12)
# Output: "Polsko"

# Możemy też użyć indeksów ujemnych
String.slice(string, -1, -5)
# Output: "ola"

# Aby wyodrębnić podciąg bez podania końcowego indeksu, użyjemy `String.slice/2`
String.slice(string, 0)
# Output: "Witaj, Polsko!"
```

## Głębsze zagadnienia

Język Elixir oferuje również inne metody wyodrębniania podciągów, takie jak `String.split/2` czy `String.split_at/2`. Ponadto, możemy również wykorzystać wyrażenia regularne do bardziej złożonych operacji na napisach. Warto również pamiętać o używaniu `String.downcase/1` lub `String.upcase/1` przy manipulowaniu tekstem.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o manipulowaniu napisami w języku Elixir, zapoznaj się z poniższymi artykułami:

- [Manipulowanie napisami w języku Elixir](https://medium.com/@kamilgrabowski/manipulowanie-tekstem-w-elixirze-7031d7323f97)
- [Podstawy wyrażeń regularnych w Elixir](https://medium.com/@lucasmaia/elixir-regular-expressions-a-quick-introduction-bb6b4c401444)