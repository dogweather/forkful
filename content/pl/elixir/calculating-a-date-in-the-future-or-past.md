---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Elixir: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co to i dlaczego?
Obliczanie daty w przyszłości lub przeszłości to proces ustalania konkretnej daty, dodając lub odejmując konkretne ilości dni, tygodni, miesięcy lub lat od/w danej dacie. Programiści robią to, aby manipulować i wykorzystywać daty w sposób istotny dla ich aplikacji, np. przewidywanie daty wygaśnięcia subskrypcji lub śledzenie okresu próbnego.

## Jak to robić:
Elixir umożliwia obliczanie dat w przyszłości lub przeszłości za pomocą wbudowanego modułu `Date`. 
Sprawdźmy przykładowy kod:
```elixir
iex> d = Date.from_iso8601!("2021-04-01")      
~D[2021-04-01]
iex> Date.add(d, 10)                       
~D[2021-04-11]
```
Pierwsza linia kodu tworzy datę "2021-04-01". W drugiej linii dodajemy 10 dni do tej daty, a wynik to "2021-04-11".

## Dogłębne zrozumienie:
W historii, do Elixir 1.3 wprowadzenie modułu `Date` uproszczone były obliczenia dat. Przed tym programiści musieli polegać na zewnętrznym kodzie lub skomplikowanych obliczeniach.

Jeśli chodzi o alternatywy, czasami może być konieczne korzystanie z modułu `DateTime` lub `NaiveDateTime` dla bardziej złożonych manipulacji z datą i czasem.

Szczegóły implementacji obejmują korzystanie z funkcji `Date.add/2`, która dodaje określoną liczbę dni do podanej daty. Jest ona zaprojektowana tak, aby poprawnie obsługiwać przestępne lata i innych skomplikowanych kalendarzowych rzeczy.

## Zobacz też:
- Elixir `Date` module: https://hexdocs.pm/elixir/Date.html 
- Elixir School lesson on `Date`, `Time` and `DateTime`: https://elixirschool.com/en/lessons/basics/date-time