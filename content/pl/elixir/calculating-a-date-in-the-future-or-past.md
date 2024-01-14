---
title:                "Elixir: Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami potrzebujemy obliczyć datę w przyszłości lub w przeszłości, na przykład w przypadku planowania wydarzeń lub tworzenia raportów. W języku Elixir istnieje wiele przydatnych funkcji, które można wykorzystać do dokonania tych obliczeń. W tym artykule dowiesz się, jak to zrobić.

## Jak zrobić

```elixir
Calendar.DateTime.add(Calendar.utc_now(), 1, :month)
```

Powyższy kod używa funkcji `add` z modułu `Calendar.DateTime`, który został importowany w ramach standardowej biblioteki `Calendar`. Pierwszym argumentem funkcji jest aktualny czas w formacie `DateTime`. Drugi argument oznacza, że chcemy dodać 1 miesiąc do bieżącego czasu. Trzeci argument przekazuje informacje, czy chcemy dodać czas lokalny (`:local`) czy czas uniwersalny (`:utc`).

Wynik powyższego kodu powinien wyglądać mniej więcej tak:

```elixir
~N[2019-06-29 15:24:50.000000] + 1 month
~N[2019-07-29 15:24:50.000000]
```

Możesz również użyć innych jednostek czasu, takich jak sekundy, minuty, godziny, dni, tygodnie i lata. Na przykład, aby obliczyć datę, która jest 2 lata w przyszłości, możesz użyć funkcji `add` w następujący sposób:

```elixir
Calendar.DateTime.add(Calendar.utc_now(), 2, :years)
```

Możesz także ustalić datę w przeszłości, zmieniając drugi argument na wartość ujemną.

## Wnikliwe spojrzenie

Obliczanie daty w przyszłości lub przeszłości odbywa się głównie dzięki funkcji `add` z modułu `Calendar`. W przypadku gdy potrzebujemy dokonać bardziej złożonych obliczeń, z pomocą przychodzi nam moduł `Timex`, który dostarcza bardziej precyzyjne i pomocne funkcje do zarządzania datami i czasem.

## Zobacz również

- [Elixir School](https://elixirschool.com/pl/) - bezpłatne i dostępne w wielu językach źródła edukacyjne dla języka Elixir.
- [Oficjalna dokumentacja Elixir](https://hexdocs.pm/elixir/DateTime.html#add/4) - pełna dokumentacja dla funkcji `add` w języku Elixir.
- [Oficjalna strona Timex](https://hexdocs.pm/timex/Timex.html) - dokumentacja i przykłady użycia dla modułu `Timex` w języku Elixir.