---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:49.334125-07:00
description: "Jak to zrobi\u0107: Utworzenie Mapy jest proste. U\u017Cywasz sk\u0142\
  adni `%{}`, tak jak tutaj."
lastmod: '2024-03-13T22:44:35.034641-06:00'
model: gpt-4-0125-preview
summary: Utworzenie Mapy jest proste.
title: Korzystanie z tablic asocjacyjnych
weight: 15
---

## Jak to zrobić:
Utworzenie Mapy jest proste. Używasz składni `%{}`, tak jak tutaj:

```elixir
my_map = %{"name" => "Alex", "age" => 32}
IO.inspect(my_map)
```

Dostęp do wartości uzyskuje się, używając kluczy:

```elixir
IO.puts my_map["name"]
```
Wyjście: `Alex`

Aby dodać lub zaktualizować wartości, można użyć funkcji `Map.put/3`:

```elixir
updated_map = Map.put(my_map, "location", "NY")
IO.inspect(updated_map)
```
Wyjście: `%{"age" => 32, "location" => "NY", "name" => "Alex"}`

Usuwanie kluczy jest równie proste dzięki `Map.delete/2`:

```elixir
trimmed_map = Map.delete(updated_map, "age")
IO.inspect(trimmed_map)
```
Wyjście: `%{"location" => "NY", "name" => "Alex"}`

## Pogłębiona analiza
Mapy w Elixirze są ewolucją starszych typów przechowywania klucz-wartość, takich jak Hashe w Ruby czy Słowniki w Pythonie. Pozwalają na bardziej efekcyjne wyszukiwanie i wstawianie, czyniąc je głównym wyborem dla nowoczesnego programowania w Elixirze. Warto zauważyć, że przed Mapami, Elixir używał modułów HashDict i Dict, które są teraz przestarzałe.

Jednakże, w scenariuszach wymagających uporządkowanych danych, można przyjrzeć się listom słów kluczowych w Elixirze. Są to listy krotek, efektywne dla mniejszych kolekcji, ale nie tak wydajne dla dużych zbiorów danych jak Mapy.

Warto mieć na uwadze, że Mapy przechowują swoje klucze w "płaskiej" strukturze, co sprawia, że bezpośredni dostęp do zagnieżdżonych wartości może być nieco trudny. Dla głębokiego zagnieżdżenia, można rozważyć ustrukturyzowany dostęp poprzez funkcje `get_in`, `put_in`, `update_in` i `get_and_update_in`, które pozwalają na bardziej dynamiczne podejście do manipulacji zagnieżdżonymi danymi.

Podsumowując, chociaż Mapy są Twoim pierwszym wyborem dla potrzeb tablic asocjacyjnych w Elixirze, język ten oferuje bogatą różnorodność struktur danych na każdą okazję, zachęcając Cię do wyboru odpowiedniego narzędzia do pracy.
