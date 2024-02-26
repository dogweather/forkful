---
date: 2024-01-27 20:33:18.574106-07:00
description: "Generowanie liczb losowych w Elixirze jest podstawowym zadaniem programistycznym,\
  \ niezb\u0119dnym dla aplikacji potrzebuj\u0105cych nieprzewidywalnych wynik\xF3\
  w,\u2026"
lastmod: '2024-02-25T18:49:33.459671-07:00'
model: gpt-4-0125-preview
summary: "Generowanie liczb losowych w Elixirze jest podstawowym zadaniem programistycznym,\
  \ niezb\u0119dnym dla aplikacji potrzebuj\u0105cych nieprzewidywalnych wynik\xF3\
  w,\u2026"
title: Generowanie liczb losowych
---

{{< edit_this_page >}}

## Co i dlaczego?

Generowanie liczb losowych w Elixirze jest podstawowym zadaniem programistycznym, niezbędnym dla aplikacji potrzebujących nieprzewidywalnych wyników, takich jak generowanie bezpiecznych tokenów, próbkowanie danych czy algorytmy gier. Programiści używają tego, aby wprowadzić poziom losowości i zmienności w swoich aplikacjach, czyniąc je bardziej dynamicznymi i mniej deterministycznymi.

## Jak to zrobić:

Aby generować liczby losowe w Elixirze, głównie używa się modułu `:rand`, który dostarcza kilka funkcji do tego celu. Oto krótki przewodnik, jak zacząć:

Najpierw upewnij się, że zainicjowałeś generator liczb losowych poprzez jego zasianie unikalnym punktem startowym:

```elixir
:rand.seed(:exsplus)
```

Aby wygenerować losową liczbę całkowitą w zakresie, użyj:

```elixir
random_integer = :rand.uniform(10) # Generuje liczbę między 1 a 10
IO.puts(random_integer)
```

Dla losowego float między 0 a 1.0:

```elixir
random_float = :rand.uniform()
IO.puts(random_float)
```

Możesz potrzebować bardziej specyficznego zakresu dla floatów, co wymaga nieco więcej obliczeń:

```elixir
min = 1.5
max = 5.5
random_float_range = min + (:rand.uniform() * (max - min))
IO.puts(random_float_range)
```

Pamiętaj, że te liczby są pseudolosowe; są określone przez ziarno i algorytm, ale wystarczają dla większości zastosowań.

## Pogłębiona analiza

Możliwości generowania liczb losowych w Elixirze opierają się na module `:rand` z Erlanga, odzwierciedlając jego dziedzictwo i bliskie związki z Erlangiem. Moduł `:rand` zastąpił starszy moduł `:random`, oferując ulepszone algorytmy generowania liczb losowych. Zapewnia on różnorodne algorytmy, domyślnie jest to `exsplus`, ale wspiera również inne, takie jak `exs64`, `exsl` i więcej, z których każdy ma swoje kompromisy w zakresie szybkości i jakości losowości.

Interesującym aspektem generowania liczb losowych w Elixirze (i tym samym w Erlangu) jest obsługa ziaren. System utrzymuje oddzielne stany ziaren dla każdego procesu, zapewniając, że równoległe procesy nie zakłócają swoich sekwencji liczb losowych. Jest to szczególnie użyteczne w aplikacjach równoległych, zapewniając przewidywalność i niezawodność w systemach rozproszonych.

Chociaż moduł `:rand` wystarcza dla większości przypadków użycia, aplikacje wymagające kryptograficznie bezpiecznych liczb losowych powinny rozważyć inne opcje. Moduł `crypto` dostarcza funkcje takie jak `crypto:strong_rand_bytes/1`, które są zaprojektowane do generowania bezpiecznych danych losowych odpowiednich do celów kryptograficznych. Te alternatywy są niezbędne dla aplikacji wrażliwych na bezpieczeństwo, takich jak generowanie tokenów, szyfrowanie i niektóre typy mechanizmów uwierzytelniania.
