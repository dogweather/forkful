---
title:                "Elixir: Generowanie losowych liczb"
programming_language: "Elixir"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego: Generowanie liczb losowych w Elixir

Generowanie liczb losowych może wydawać się niepotrzebnym lub przypadkowym aspektem programowania, ale jest niezwykle ważnym elementem w wielu aplikacjach. Przykładowo, losowe liczby mogą być wykorzystane do losowania zwycięzców w konkursach, generowania unikalnych identyfikatorów lub testowania funkcji w kodzie. W Elixir, istnieje wiele sposobów na generowanie liczb losowych, a w tym artykule opiszemy najpopularniejsze metody.

## Jak to zrobić: Przykłady kodu i rezultaty

### Używając funkcji `Enum.random/1`

Jednym z najprostszych sposobów na wygenerowanie losowej liczby jest użycie funkcji `Enum.random/1` z modułu `Enum`. Ta funkcja pobiera kolekcję i zwraca losowy element z tej kolekcji. W przypadku pobrania pojedynczej liczby, funkcja wylosuje ją z przedziału od 0 do podanej liczby.

```Elixir
Enum.random(100)
# => 57
```

### Używając biblioteki `:rand`

Elixir udostępnia również wbudowaną bibliotekę `:rand`, która oferuje więcej zaawansowanych funkcji generowania liczb losowych. Jedną z nich jest funkcja `random_uniform/1`, która wylosuje liczbę z przedziału od 0 do podanej liczby.

```Elixir
:rand.uniform(100)
# => 33
```

### Używając funkcji `Kernel.SpecialForms.apply/2`

Możemy również wykorzystać funkcję `apply/2` z modułu `Kernel.SpecialForms`, aby wywołać dowolną funkcję i przekazać jej jako argument wartości losowe. W poniższym przykładzie wywołujemy funkcję `Enum.random/1` i przekazujemy jej przedział od 0 do 100.

```Elixir
apply(Enum, :random, [100])
# => 91
```

## Głębszy zanurzenie: Dodatkowe informacje o generowaniu liczb losowych

### Losowanie z różnych rozkładów

Poza standardowym wyborem losowych liczb, możemy również wykorzystać funkcję `:rand.uniform/1` do losowania z określonego rozkładu prawdopodobieństwa. W poniższym przykładzie losujemy z rozkładu normalnego o średniej 10 i odchyleniu standardowym 5.

```Elixir
:rand.normal(10, 5)
# => 7.524284
```

### Używanie własnego generatora liczb losowych

Jeśli chcemy wykorzystać własny generator liczb losowych, możemy zdefiniować własny moduł i wykorzystać go w naszym kodzie. Moduł ten musi implementować funkcję `:rand.uniform/0`, która zwraca losową wartość z przedziału od 0 do 1, oraz funkcję `:rand.uniform/1`, która przyjmuje argument powinna zwracać losową wartość z przedziału od 0 do tej liczby.

```Elixir
defmodule MyRandom do
  defuniform do
    :ok
    if(:rand.uniform < 0.5, do: 1, else: 2)
  end
  defuniform(n) do
    :ok
    if(:rand.uniform < 0.5, do: n * 2, else: n * 3)
  end
end

MyRandom.uniform
# => 1 lub 2
MyRandom.uniform(10)
# => 20 lub 30
```

## Zobacz też

- [Dokumentacja Elixir: Generowanie liczb losowych](https://hexdocs.pm/elixir/Kernel.html#uniform/1)
- [Poradnik programowania: Jak generować losowe liczby w Elixir](https://www.brianstorti.com/random-numbers-in-elixir/)