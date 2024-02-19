---
aliases:
- /pl/ruby/printing-debug-output/
date: 2024-01-20 17:53:24.494309-07:00
description: "Wypisywanie informacji diagnostycznych to spos\xF3b na wy\u015Bwietlanie\
  \ danych w trakcie dzia\u0142ania programu, \u017Ceby zrozumie\u0107, co si\u0119\
  \ w nim dzieje. Debugowanie\u2026"
lastmod: 2024-02-18 23:08:50.128176
model: gpt-4-1106-preview
summary: "Wypisywanie informacji diagnostycznych to spos\xF3b na wy\u015Bwietlanie\
  \ danych w trakcie dzia\u0142ania programu, \u017Ceby zrozumie\u0107, co si\u0119\
  \ w nim dzieje. Debugowanie\u2026"
title: "Drukowanie komunikat\xF3w debugowania"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wypisywanie informacji diagnostycznych to sposób na wyświetlanie danych w trakcie działania programu, żeby zrozumieć, co się w nim dzieje. Debugowanie jest nieodłączne od programowania – pomaga znajdować i naprawiać błędy.

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
