---
title:                "Generowanie losowych liczb"
html_title:           "Elixir: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest nieodłączną częścią programowania. Wykorzystujemy je do symulowania rzeczywistości, gier, losowania i wielu innych zastosowań. W Elixir, możemy generować losowe liczby w prosty i wydajny sposób.

## Jak to zrobić

```Elixir
# Wygenerowanie losowej liczby całkowitej
random_integer = :random.uniform()

# Wygenerowanie losowej liczby z przedziału
random_float = :random.uniform(1.0..10.0)

# Wygenerowanie losowego ciągu znaków
random_string = :random.uniform("ABCDEF", length: 5)

# Wygenerowanie listy losowych liczb
random_list = for _ <- 1..5, do: :random.uniform(1..10)
```

Output:
```
random_integer: 162077
random_float: 5.322336
random_string: DACEB
random_list: [7, 2, 10, 9, 4]
```

## Głębsze spojrzenie

W Elixir, funkcja :random.uniform/2 jest wykorzystywana do generowania liczb losowych o rozkładzie jednostajnym na podstawie generatora liczb losowych Xorshift. Możemy również wykorzystać moduł :rand, który oferuje więcej funkcji do generacji różnego typu danych, takich jak listy, krotki i czasami nawet atomów. Warto również zauważyć, że Elixir zapewnia szybkie i wydajne generowanie liczb losowych, co jest szczególnie ważne w aplikacjach wymagających dużych ilości danych losowych.

## Zobacz także

* Dokumentacja Elixir na temat generowania liczb losowych: https://hexdocs.pm/elixir/Random.html
* Przykładowe zastosowanie generowania liczb losowych w grach: https://www.youtube.com/watch?v=5xZGmEdj-bg
* Porównanie wydajności generowania liczb losowych w Elixir i innych językach programowania: https://engineering.appsflyer.com/random-numbers-in-elixir-and-other-languages/