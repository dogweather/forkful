---
date: 2024-01-26 03:44:09.426663-07:00
description: "Zaokr\u0105glanie liczb oznacza dostosowanie ich do bliskiej warto\u015B\
  ci dla uproszczenia lub aby dopasowa\u0107 je do okre\u015Blonej precyzji. Jest\
  \ to przydatne do\u2026"
lastmod: '2024-02-25T18:49:33.458577-07:00'
model: gpt-4-0125-preview
summary: "Zaokr\u0105glanie liczb oznacza dostosowanie ich do bliskiej warto\u015B\
  ci dla uproszczenia lub aby dopasowa\u0107 je do okre\u015Blonej precyzji. Jest\
  \ to przydatne do\u2026"
title: "Zaokr\u0105glanie liczb"
---

{{< edit_this_page >}}

## Co i dlaczego?
Zaokrąglanie liczb oznacza dostosowanie ich do bliskiej wartości dla uproszczenia lub aby dopasować je do określonej precyzji. Jest to przydatne do poprawy czytelności, zmniejszenia zajmowanego miejsca w pamięci lub zaspokojenia specyficznych potrzeb domenowych, takich jak obliczenia pieniężne, gdzie chcesz zaokrąglić do najbliższego centa.

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
