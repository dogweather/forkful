---
title:                "Ruby: Wydrukowanie wyników debugowania."
programming_language: "Ruby"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Każdy programista, niezależnie od stopnia zaawansowania, musi czasem śledzić kod i znaleźć błędy. Właśnie wtedy przydaje się wydrukowanie informacji na temat przebiegu kodu, czyli tzw. "debug output". Pozwala to na lepsze zrozumienie działania programu i szybsze znalezienie ewentualnych błędów.

## Jak to zrobić

Najprostszym sposobem na wyświetlenie debug output jest użycie metody "puts", która wypisze przekazany jej argument na ekranie. Możemy także wykorzystać specjalną metodę "p" lub "print", która również można znaleźć w dokumentacji języka Ruby.

```Ruby
puts "Hello World"
p "Debug output"
print "Output without newline"
```

W powyższych przykładach widzimy, że każda z tych metod działa w podobny sposób, ale ma pewne różnice, które mogą mieć znaczenie w zależności od sytuacji.

## Głębszy zanurzenie

Ponieważ wyświetlanie debug output jest tak powszechnie stosowaną techniką, istnieje wiele sposobów na dostosowanie go do własnych potrzeb. Na przykład możemy użyć metody "p" zamiast "puts" w celu uzyskania dokładniejszej informacji na temat typu i wartości wyświetlanych danych. Możemy również dodać warunek, dzięki któremu debug output będzie wyświetlany tylko wtedy, gdy będzie spełniony.

```Ruby
p 123                 # wyświetli: 123
puts [1, 2, 3]        # wyświetli: 1
                      #            2
                      #            3
puts 123 if true      # wyświetli: 123

p 123 if false        # nie wyświetli niczego
```

Ponadto, poza standardowym wyświetlaniem danych, możemy również korzystać z różnych gemów i bibliotek, które oferują bardziej zaawansowane metody debugowania. Przykładem może być popularny gem "pry", który umożliwia interaktywne wyświetlanie i manipulację danymi w czasie wykonywania programu.

## Zobacz także

- ["Debugowanie w Ruby" na stronie DZone](https://dzone.com/articles/a-debugging-tutorial-for-ruby)
- ["Praktyczny poradnik debugowania w Ruby" na stronie Medium](https://medium.com/rubyinside/practical-guide-debugging-ruby-b6f6db8dbc97)
- ["Przydatne techniki debugowania w Ruby" na stronie RubyGuides](https://www.rubyguides.com/2019/09/ruby-debugging/)