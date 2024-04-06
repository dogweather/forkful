---
date: 2024-01-20 17:53:24.494309-07:00
description: "Jak to zrobi\u0107: Historia wypisywania informacji diagnostycznych\
  \ si\u0119ga pocz\u0105tk\xF3w programowania \u2013 by\u0142o to niezb\u0119dne\
  \ do zrozumienia, co robi\u0142y pierwsze\u2026"
lastmod: '2024-04-05T22:50:50.286427-06:00'
model: gpt-4-1106-preview
summary: "Historia wypisywania informacji diagnostycznych si\u0119ga pocz\u0105tk\xF3\
  w programowania \u2013 by\u0142o to niezb\u0119dne do zrozumienia, co robi\u0142\
  y pierwsze komputery."
title: "Drukowanie komunikat\xF3w debugowania"
weight: 33
---

## Jak to zrobić:
```Ruby
# prosty przykład wypisywania komunikatu
puts "Hej, to ja, Twój program!"

# pokazanie wartości zmiennej
zmienna = "Tajemnica"
puts "Wartość zmiennej: #{zmienna}"

# używanie p dla lepszego debugowania - wypisuje dane w postaci użytecznej dla programisty
p "Wypisane przez 'p': #{zmienna}"
```
Output:
```
Hej, to ja, Twój program!
Wartość zmiennej: Tajemnica
"Wypisane przez 'p': Tajemnica"
```

## Głębsze spojrzenie:
Historia wypisywania informacji diagnostycznych sięga początków programowania – było to niezbędne do zrozumienia, co robiły pierwsze komputery. W Ruby, metoda `puts` jest najczęściej używanym sposobem na wypisywanie treści, ale istnieją alternatywy takie jak `print` (bez nowej linii na końcu) czy `p` (która zwraca wartość tak jak wygląda w kodzie, włącznie z typami danych).

Warto też zwrócić uwagę na bardziej zaawansowane narzędzia jak `logger`, które pozwalają kontrolować poziom szczegółowości logów oraz miejsce ich zapisu (np. do pliku zamiast na ekran). W niektórych przypadkach używa się także zewnętrznych gemów jak `pry` do interaktywnego debugowania.

## Zobacz również:
- [Ruby-Doc dla metody puts](https://ruby-doc.org/core-2.7.0/IO.html#method-i-puts)
- [Ruby-Doc dla metody p](https://ruby-doc.org/core-2.7.0/Kernel.html#method-i-p)
- [Dokumentacja Ruby logger](https://ruby-doc.org/stdlib-2.5.1/libdoc/logger/rdoc/Logger.html)
