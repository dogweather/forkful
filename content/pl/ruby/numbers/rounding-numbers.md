---
title:                "Zaokrąglanie liczb"
aliases:
- /pl/ruby/rounding-numbers/
date:                  2024-01-26T03:47:03.578337-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zaokrąglanie liczb"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/rounding-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Zaokrąglanie liczb oznacza dostosowanie ich do najbliższej liczby całkowitej lub do określonego stopnia dokładności. Programiści zaokrąglają liczby, aby upraszczać, dostosowywać do oczekiwań ludzkich, lub dostosowywać dane do konkretnych formatów—pomyśl o obliczeniach finansowych, prezentacjach graficznych lub redukcji rozmiaru danych.

## Jak to zrobić:

```Ruby
# Podstawowe zaokrąglanie
puts 3.14159.round      # => 3
puts 2.6.round          # => 3

# Określanie dokładności
puts 3.14159.round(2)   # => 3.14
puts 2.675.round(2)     # => 2.68

# Zaokrąglanie w dół
puts 2.9.floor          # => 2

# Zaokrąglanie w górę
puts 2.1.ceil           # => 3

# Zaokrąglanie w stronę zera
puts -2.9.round         # => -3
puts -2.9.truncate      # => -2
```

Wynik przykładowy:
```
3
3
3.14
2.68
2
3
-3
-2
```

## Szczegółowe omówienie
Zaokrąglanie liczb nie jest czymś nowym—ludzie robili to od wieków, aby ułatwić sobie obliczenia lub pracować w granicach możliwości swoich narzędzi. W Ruby, metoda `round` jest wszechstronna, z możliwością zaokrąglania do najbliższej liczby całkowitej domyślnie lub do określonego miejsca dziesiętnego.

Alternatywą dla `round` jest `floor` dla zawsze zaokrąglania w dół, i `ceil` dla zawsze zaokrąglania w górę, niezależnie od wartości liczby. aby po prostu odciąć miejsca dziesiętne, masz do dyspozycji `truncate`.

Historycznie, jeśli chodzi o komputery, zaokrąglanie staje się kluczowe przy obchodzeniu się z arytmetyką zmiennoprzecinkową ze względu na jej nieodłączną niedokładność. Ruby, jak większość języków, podąża za standardem IEEE 754 dla liczb zmiennoprzecinkowych, co oznacza, że obsługuje zaokrąglanie w sposób, który większość programistów powinna być w stanie przewidzieć i na który powinna móc polegać.

Jest jednak więcej—rzeczy takie jak zaokrąglanie bankowe (znane również jako zaokrąglanie do najbliższej parzystej) to koncepcje, które programiści Ruby mogą potrzebować zaimplementować ręcznie, ponieważ metoda `round` nie oferuje tego z marszu.

## Zobacz również
- [Dokumentacja Ruby](https://ruby-doc.org/core-3.0.0/Float.html#method-i-round) dla metody `round` Floats.
- [Standard IEEE dla arytmetyki zmiennoprzecinkowej (IEEE 754)](https://ieeexplore.ieee.org/document/4610935).
- [Zrozumienie dokładności zmiennoprzecinkowej](https://floating-point-gui.de/), dla głębszego wglądu w to, jak komputery obsługują liczby dziesiętne.
