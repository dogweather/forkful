---
title:    "Elixir: Generowanie losowych liczb"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest ważną częścią wielu programów i aplikacji. Może to być wykorzystane do symulacji, do gier komputerowych lub do stworzenia unikalnych identyfikatorów. W tym artykule dowiesz się, jak w prosty sposób generować losowe liczby w języku Elixir.

## Jak to zrobić

Aby wygenerować losową liczbę w Elixir, możemy skorzystać z funkcji `:rand.seed/1`, która ustawia ziarno dla generatora liczb pseudolosowych. Następnie możemy użyć funkcji `:rand.uniform/2` do wygenerowania losowej liczby z podanego przedziału.

```Elixir
:rand.seed(:os.system_time(:millisecond))

:rand.uniform(1, 10)
# output: 5
```

Możemy także wykorzystać funkcję `:rand.uniform/1` aby wygenerować losową liczbę z zakresu od 0 do podanej wartości.

```Elixir
:rand.uniform(100)
# output: 57
```

Jeśli chcemy wygenerować losowy ciąg znaków, możemy skorzystać z funkcji `:rand.uniform/0`, która zwróci liczbę typu float z zakresu od 0.0 do 1.0.

```Elixir
:rand.uniform()
# output: 0.4928907669469717
```

W razie potrzeby możemy także wykorzystać funkcję `:rand.uniform/3` do wygenerowania wielu losowych liczb w jednym wywołaniu.

```Elixir
:rand.uniform(1..10, 5)
# output: [9, 6, 4, 2, 5]
```

## Zagłębienie się

Generator liczb pseudolosowych w języku Elixir jest oparty na algorytmie Mersenne Twister. Jest to popularna metoda generowania liczb pseudolosowych, która jest szybka i wydajna. Możemy skorzystać z funkcji `:rand.seed/1` do ustawienia ziarna według naszych potrzeb, na przykład na podstawie wyniku innej funkcji lub liczby sekund od danego punktu w czasie.

Zwróć uwagę, że funkcje `:rand.uniform/1` i `:rand.uniform/3` zwracają wartości zmiennoprzecinkowe, więc jeśli potrzebujemy liczb całkowitych, musimy skorzystać z funkcji `round/1`. Możemy także wykorzystać funkcję `:rand.seed/1` w taki sposób, aby uniknąć duplikatów generowanych liczb.

## Zobacz też

- [Dokumentacja Elixir do funkcji :rand](https://hexdocs.pm/elixir/Kernel.SpecialForms.html#rand/0)
- [Wszystko, co musisz wiedzieć o generowaniu liczb pseudolosowych](https://dev.to/shadowfaxtech/all-you-need-to-know-about-pseudo-random-generators-3j0m)
- [Wykorzystywanie funkcji :rand w języku Elixir](https://elixir-lang.org/getting-started/randomness.html)