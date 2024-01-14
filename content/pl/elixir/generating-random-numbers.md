---
title:                "Elixir: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Dlaczego generowanie losowych liczb jest ważne w Elixirze?

Generowanie losowych liczb jest istotnym aspektem w każdym języku programowania, a Elixir nie jest wyjątkiem. Dzięki generowaniu losowych liczb możemy symulować różnorodne scenariusze i testować nasze aplikacje pod różnymi warunkami. Pozwala to również na tworzenie unikatowych kluczy, hasła czy identyfikatorów dla naszych użytkowników.

# Jak wygenerować losowe liczby w Elixirze?

Możemy wykorzystać wbudowaną funkcję `:rand.uniform/1` do generowania losowych liczb lub wykorzystać moduł `:rand` w celu dostosowania generowanych liczb do naszych potrzeb.

```Elixir
# Prosty przykład generowania losowego numeru między 1 a 100
rand_num = :rand.uniform(1..100)
IO.puts(rand_num)

# Przykład wykorzystania modułu `:rand` do wygenerowania losowego hasła
require Integer
require List

alphabet = 'abcdefghijklmnoprstuwxyz'
user_password = :rand.seed(:erlang.timestamps)
|> Integer.to_string(36)
|> String.codepoints()
|> List.to_tuple()
|> Enum.map(fn(x) -> alphabet[x] end)
|> List.to_string()

IO.puts(user_password)
```

### Output:
```
47
v7mhnspz
```

# Głębszy wgląd w generowanie losowych liczb

Funkcja `:rand.uniform/1` wykorzystuje generator pseudolosowy o nazwie Mersenne Twister. Jest to algorytm o bardzo dużej periodyczności, co oznacza, że wygenerowane liczby są trudne do przewidzenia, ale jednak nie są całkowicie losowe. Dlatego zaleca się wykorzystywanie tego typu generatorów jedynie do celów niedokładnych symulacji lub przy generowaniu wartości, które nie są krytyczne dla bezpieczeństwa naszej aplikacji.

Możemy również używać modułu `:rand` w celu dostosowania naszego generatora pseudolosowego do bardziej konkretnych zastosowań. Na przykład, możemy określić ziarno dla generatora, aby uzyskać te same wyniki zawsze, gdy generujemy numery z tych samych danych wejściowych. Możemy również wybrać spośród kilku różnych algorytmów generujących do wykorzystania w zależności od naszych wymagań.

# Zobacz również
- Dokumentacja Elixir dla funkcji `:rand.uniform/1`: https://hexdocs.pm/elixir/1.10/Kernel.html#base-uniform/2
- Dokumentacja Elixir dla modułu `:rand`: https://hexdocs.pm/elixir/1.10/Random.html
- Konwersja liczby całkowitej na ciąg znaków w Elixirze: https://elixirschool.com/pl/lessons/basics/basics/#właściwość-wewnątrz-właściwości-string-codepointsi-listtostring/
- Przewidywalność pseudolosowych liczb w informatyce: https://en.wikipedia.org/wiki/Pseudorandomness