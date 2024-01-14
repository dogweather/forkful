---
title:    "Elixir: Generowanie losowych liczb"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest istotnym elementem wielu programów i aplikacji, ponieważ pozwala na tworzenie przypadkowych zdarzeń, symulacji oraz testów. W języku Elixir istnieją wygodne funkcje, które umożliwiają generowanie losowych liczb w prosty i skuteczny sposób.

## Jak to zrobić

Aby wygenerować losową liczbę w języku Elixir, możemy skorzystać z funkcji ```Elixir:random.uniform/1```, która zwraca liczbę z przedziału od 0 do 1. Przykładowy kod wyglądałby następująco:

```Elixir
random.uniform()
```

Wynik działania tej funkcji możemy przypisać do zmiennej, aby móc dalej z niej korzystać. Na przykład:

```Elixir
my_random_number = random.uniform()
# Wynik: 0.534397
```

Jeśli chcemy wygenerować losową liczbę z określonego przedziału, możemy skorzystać z funkcji ```Elixir:random.uniform/2```, podając jako argumenty dolną i górną granicę przedziału. Przykładowy kod wyglądałby następująco:

```Elixir
random.uniform(1, 10)
# Wynik: 7.23423
```

Kolejną przydatną funkcją jest ```Elixir:random.seed/1```, która pozwala na ustawienie ziarna do generowania liczb losowych. Dzięki temu możemy kontrolować powtarzalność wyników dla danego ziarna. Przykładowy kod wyglądałby następująco:

```Elixir
random.seed(1234)
my_random_number = random.uniform()
# Wynik: 0.101076
```

## Wnikliwa analiza

W języku Elixir, generowanie liczb losowych odbywa się za pomocą generatora liczb pseudolosowych, który jest oparty na algorytmie Mersenne Twister. Algorytm ten jest szybki, wydajny i zapewnia duży zakres liczb losowych. Dzięki funkcji ```Elixir:random.seed/1```, możemy kontrolować powtarzalność wyników, co jest przydatne w testowaniu i debugowaniu kodu.

## Zobacz również

- Dokumentacja języka Elixir dotycząca generowania liczb losowych: https://elixir-lang.org/getting-started/random-numbers.html
- Poradnik na temat wykorzystania funkcji generujących losowe liczby w Elixir: https://dev.to/itseranga/generate-random-numbers-in-elixir-3oc
- Przydatna biblioteka do generowania liczb losowych w Elixir: https://github.com/artemeff/rand