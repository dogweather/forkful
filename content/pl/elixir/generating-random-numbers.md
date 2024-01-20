---
title:                "Generowanie liczb losowych"
html_title:           "Gleam: Generowanie liczb losowych"
simple_title:         "Generowanie liczb losowych"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Generowanie liczb losowych polega na tworzeniu ciągu liczb, które nie mają przewidywalnego wzorca. Programiści wykonują to zadanie, aby symulować zjawiska naturalne, generować unikalne identyfikatory lub tworzyć trudne do odgadnięcia dane szyfrowane. 

## Jak to zrobić:
W Elixir generowanie liczb losowych jest proste. Załóżmy, że chcemy wygenerować liczbę losową między 0 a 10.

```Elixir
IO.inspect(Enum.random(0..10))
```

Przykładowe wyjście po uruchomieniu powyższego kodu to:
```Elixir
7
```
Przy każdym uruchomieniu kodu, wartość może się różnić, ponieważ jest to liczba losowa!

## Podróż w głąb tematu
Generowanie liczb losowych to stary koncept w dziedzinie informatyki. W dawniejszych algorytmach, na przykład w języku C, liczbę losową generowano na podstawie czasu systemowego. Jednak Elixir, język programowania funkcjonalnego, korzysta z wbudowanej funkcji Enum.random, co ułatwia pracę.

Alternatywą dla Enum.random może być korzystanie z :rand.uniform w Elixirze. Na przykład, :rand.uniform(10) wygeneruje losową liczbę od 1 do 10.

Ważne jest jednak, że "losowe" liczby generowane przez komputery są de facto pseudolosowe, co oznacza, że są one wygenerowane na podstawie określonego (choć skomplikowanego) algorytmu.