---
date: 2024-01-26 03:44:09.426663-07:00
description: "Jak to zrobi\u0107: W Elixirze mo\u017Cesz u\u017Cy\u0107 `Float.round/2`,\
  \ aby zaokr\u0105gli\u0107 liczb\u0119 zmiennoprzecinkow\u0105. Mo\u017Cesz okre\u015B\
  li\u0107 liczb\u0119 cyfr po przecinku, kt\xF3re chcesz\u2026"
lastmod: '2024-03-13T22:44:35.036625-06:00'
model: gpt-4-0125-preview
summary: "W Elixirze mo\u017Cesz u\u017Cy\u0107 `Float.round/2`, aby zaokr\u0105gli\u0107\
  \ liczb\u0119 zmiennoprzecinkow\u0105."
title: "Zaokr\u0105glanie liczb"
weight: 13
---

## Jak to zrobić:
W Elixirze możesz użyć `Float.round/2`, aby zaokrąglić liczbę zmiennoprzecinkową. Możesz określić liczbę cyfr po przecinku, które chcesz zachować. Oto jak to działa:

```elixir
# Zaokrąglij liczbę do miejsc po przecinku równych 0
Float.round(3.14159) # => 3.0

# Zaokrąglij liczbę do 2 miejsc po przecinku
Float.round(3.14159, 2) # => 3.14

# Zaokrąglij liczbę z ujemną precyzją do najbliższej 10
Float.round(123.456, -1) # => 120.0
```

## Wnikliwie
Zaokrąglanie liczb to klasyczny problem w informatyce — na tyle, że wybór strategii zaokrąglania może wpłynąć na systemy finansowe, obliczenia naukowe i więcej. Domyślnie `Float.round/2` w Elixirze używa zaokrąglania "w górę", przypominającego tradycyjne zaokrąglanie nauczane na matematyce.

Jeśli potrzebujesz innych typów zaokrąglania, Elixir pozwala stworzyć własne. Rozważ na przykład zaokrąglanie „w dół” (zawsze w dół) lub zaokrąglanie „w górę” (zawsze w górę). Użyjesz do tego `Float.floor/1` lub `Float.ceil/1`, odpowiednio.

```elixir
# Zaokrąglanie w dół
Float.floor(3.999) # => 3.0

# Zaokrąglanie w górę
Float.ceil(3.001) # => 4.0
```

Te alternatywy pomagają dopasować zaokrąglanie do dokładnych potrzeb twojej aplikacji, czy to obliczenia finansowe, renderowanie grafiki czy przybliżanie danych.

## Zobacz również
Aby dowiedzieć się więcej o funkcjach zaokrąglających w Elixirze i liczbach zmiennoprzecinkowych:

- Oficjalna dokumentacja Elixira dotycząca `Float`: https://hexdocs.pm/elixir/Float.html
- IEEE Standard dla arytmetyki zmiennoprzecinkowej (IEEE 754): https://ieeexplore.ieee.org/document/4610935
