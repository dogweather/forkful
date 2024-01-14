---
title:                "Elixir: Wydrukowanie danych debugowania"
simple_title:         "Wydrukowanie danych debugowania"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego warto używać drukowania danych debugowania w Elixir
Drukowanie danych debugowania jest kluczowym narzędziem w każdym języku programowania, w tym również w Elixir. Pozwala ono na monitorowanie i analizę przepływu danych w trakcie działania aplikacji, co pozwala programistom na szybsze rozwiązywanie problemów i poprawianie błędów. Jest to nieodłączna część procesu tworzenia oprogramowania i warto znać odpowiednie sposoby jego wykorzystania.

## Jak używać drukowania danych debugowania w Elixir
W Elixirze można drukować dane debugowania przy użyciu funkcji `IO.inspect/2`, która pozwala na wyświetlanie wartości różnych typów danych w konsoli. Przykładowe użycie tej funkcji wygląda następująco:

```
Elixir IO.inspect
1
2
3
4
```

#> 4

#> 3

#> 2

#> 1

Można również użyć opcji `:label` do dodania nazwy dla danych, co ułatwia późniejsze odnalezienie w konsoli.

```
Elixir IO.inspect
1
2, label: :liczba

#> liczba: 2
```

W przypadku potrzeby drukowania większej ilości danych, można użyć funkcji `IO.puts/1`, która wyświetla dane w formacie tekstowym. Przykładowe użycie:

```
Elixir IO.puts
"Hello, debug!"
```

#> Hello, debug!

## Głębokie zagłębienie w drukowaniu danych debugowania
Ponieważ Elixir jest językiem funkcyjnym, warto mieć na uwadze kilka szczególnych przypadków, które mogą wpłynąć na poprawność drukowania danych debugowania. W przypadku korzystania z funkcji `IO.inspect/2`, należy pamiętać, że argumenty są przekazywane do niej za pomocą odwołania (ang. reference), co oznacza, że drukowane dane mogą nie być najświeższe. W przypadku, gdy chcemy wyświetlić aktualną wartość zmiennej, należy użyć operatora `^`.

```
Elixir IO.inspect
^zmienna
```

Kolejnym istotnym aspektem jest korzystanie z funkcji wewnątrz innych funkcji, co może skutkować niestandardowym wyświetlaniem danych debugowania. W takiej sytuacji, warto skorzystać z opcji `label` lub użyć funkcji `IO.puts/1` do wyświetlenia danych w postaci tekstu.

## Zobacz także
- Domyślna dokumentacja Elixir dla funkcji IO.inspect: https://hexdocs.pm/elixir/IO.html#inspect/2
- Blogowy wpis o korzystaniu z drukowania debugowania w Elixir: https://akouti.com/2016/04/top-5-alternative-phoenix-debugging-techniques/
- Video tutorial o drukowaniu danych debugowania w Elixir: https://www.youtube.com/watch?v=Up1F7homv5g