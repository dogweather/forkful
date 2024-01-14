---
title:    "Ruby: Wydrukuj wyjście debugowania"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

W ciągu naszej kariery programistycznej często napotykamy na błędy w kodzie, które trudno jest nam zlokalizować. W takich przypadkach bardzo przydatne jest wypisywanie wiadomości debugujących, dzięki którym możemy śledzić działanie programu i zidentyfikować problem. W tym wpisie dowiesz się, jak skutecznie używać funkcji wypisywania debugu w języku Ruby.

## Jak to zrobić

W celu wypisania wiadomości debugujących w Ruby, możemy skorzystać z metody `puts` lub `p`. Poniższy przykład wypisuje zmienną `x` na ekran:

```Ruby
x = 5
puts x            # Output: 5
p x               # Output: 5
```

Warto również wiedzieć, że `p` wypisuje również typ danych danej zmiennej:

```Ruby
x = 5
puts x            # Output: 5
p x               # Output: Integer(5)
```

Możemy również dodać wiadomość w celu lepszej orientacji w outputcie:

```Ruby
x = 5
puts "Wartość zmiennej x to #{x}."
p "Wartość zmiennej x to #{x}."  # Output: "Wartość zmiennej x to 5."
```

Możliwe jest również używanie wielu wyrażeń w jednej linii:

```Ruby
x = 5
y = 10
puts "Suma x i y to #{x + y}."
p "Suma x i y to #{x + y}."  # Output: "Suma x i y to 15."
```

Jeśli chcemy dodać więcej informacji do wiadomości debugujących, możemy wykorzystać specjalną składnię `<<`:

```Ruby
x = 5
y = 10
puts "Wartość x: #{x} i y: #{y}."
p "Wartość x: #{x} i y: #{y}."
puts <<~DEBUG              # Output: "Wartość x: 5 i y: 10."
#{x} + #{y} to #{x + y}.
DEBUG
p <<~DEBUG                # Output: "Wartość x: 5 i y: 10.\n15"
#{x} + #{y} to #{x + y}.
DEBUG
```

## Deep Dive

Podczas wypisywania debug outputu warto pamiętać o kilku ważnych aspektach. Po pierwsze, nie należy nadużywać tej funkcji, ponieważ może ona spowolnić działanie naszego programu. Najlepiej używać jej tylko w wyjątkowych sytuacjach, kiedy faktycznie jest to potrzebne.

Warto również starannie dobierać wiadomości debugujące, aby były czytelne i łatwe do zrozumienia. Możemy np. dodawać informacje o aktualnej wartości zmiennych lub miejscu w kodzie, w którym została wywołana wiadomość debugująca. Dzięki temu w razie potrzeby łatwiej będzie nam się odnaleźć w kodzie.

## Zobacz również

- [Ruby Dokumentacja](https://ruby-doc.org/core-2.7.2/Kernel.html#method-i-p)
- [Wypisywanie debug outputu w języku Ruby](https://www.theodinproject.com/paths/full-stack-ruby-on-rails/courses/ruby-programming/lessons/debugging)
- [Przewodnik dla początkujących w języku Ruby](https://www.rubyguides.com/2019/01/p-print-and-puts/)