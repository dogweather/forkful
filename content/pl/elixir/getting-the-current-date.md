---
title:    "Elixir: Pobieranie bieżącej daty"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Dlaczego warto poznać bieżącą datę w Elixirze?

Poznanie bieżącej daty może być niezbędne w wielu projektach związanych z programowaniem. W Elixirze istnieje wiele sposobów na pobranie obecnego czasu, w tym kilka wbudowanych funkcji, które ułatwiają to zadanie. W tym artykule pokażemy Ci, dlaczego i jak można poznać bieżącą datę w Elixirze.

## Jak to zrobić?

Pierwszą i najprostszą metodą pobrania bieżącego czasu w Elixirze jest użycie funkcji `DateTime.utc_now()`. Ta funkcja zwraca obecny czas w strefie czasowej UTC w formacie `DateTime`.

```Elixir
DateTime.utc_now()
```
Przykładowy output:
```
#DateTime<2020-07-28 15:45:23.182827Z>
```
Jeśli chcemy otrzymać bieżący czas w naszej lokalnej strefie czasowej, możemy użyć funkcji `DateTime.local_now()`.

```Elixir
DateTime.local_now()
```
Przykładowy output:
```
#DateTime<2020-07-28 11:45:23.182827-04:00 America/New_York>
```

Inną opcją jest użycie funkcji `System.system_time()` lub `System.system_time(:second)`, aby otrzymać czas w sekundach od 1 stycznia 1970 roku (epoch time).

```Elixir
System.system_time()
```
Przykładowy output:
```
1595937880
```
## Zagłębienie się w temat

Teraz, gdy już wiesz, jak pobierać obecny czas w Elixirze, warto dowiedzieć się więcej o rodzajach danych, które są zwracane przez te funkcje. Funkcja `DateTime.utc_now()` zwraca wartość typu `DateTime`, który jest wykorzystywany do przechowywania daty i czasu w formacie względnym lub bezwzględnym. Możemy także wykorzystać funkcję `NaiveDateTime.utc_now()` lub `NaiveDateTime.local_now()`, aby otrzymać datę i czas w postaci niepowiązanej ze strefami czasowymi.

Funkcja `System.system_time()` zwraca wartość typu `integer`, która może być wykorzystana do obliczeń lub konwersji na bardziej złożone obiekty daty i czasu. Możemy również użyć funkcji `:os.system_time()` lub `:os.system_time(:second)`, aby otrzymać czas w formacie zgodnym z systemem operacyjnym.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o funkcjach i typach danych związanych z datami i czasem w Elixirze, warto zajrzeć na następujące strony:

- Dokumentacja Elixir: [DateTime](https://hexdocs.pm/elixir/DateTime.html)
- Blog Elixir School: [Date and Time in Elixir](https://elixirschool.com/en/lessons/advanced/date-time/)