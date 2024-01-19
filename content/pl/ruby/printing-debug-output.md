---
title:                "Drukowanie komunikatów debugowania"
html_title:           "Haskell: Drukowanie komunikatów debugowania"
simple_title:         "Drukowanie komunikatów debugowania"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Drukowanie danych do debugowania to technika programistyczna polegająca na wyświetlaniu wartości zmiennych w celu sprawdzenia ich stanu podczas wykonania kodu. Programiści robią to, aby łatwiej zrozumieć jak działa program i zidentyfikować i naprawić błędy.

## Jak to zrobić:

W Ruby używamy zwyczajowo metody `puts` do wydrukowania danych do debugowania. Poniżej znajduje się przykładowy kod:

```Ruby
def divide(num1, num2)
  puts "Dividing #{num1} by #{num2}"
  result = num1 / num2
  puts "Result is #{result}"
rescue ZeroDivisionError
  puts "Cannot divide by zero!"
end
```

Gdy uruchomimy powyższy kod z parametrami `divide(10, 2)`, zobaczymy na ekranie:

```
Dividing 10 by 2
Result is 5
```

## Głębsze spojrzenie:

Historia debugowania rozpoczęła się od manualnego śledzenia wartości zmiennych na papierze. Na szczęście, dzisiaj mamy do dyspozycji narzędzia takie jak "puts". Naturalnie istnieją alternatywy dla `puts`, takie jak `p`, `print`, `printf` oraz `pp`.

Szczegóły implementacji `puts` mogą różnić się w zależności od użytego interpreta – na przykład standardowy ruby (MRI) asocjuje `puts` z wyjściem `STDOUT`, jednak niektóre implementacje mogą dokonać innej asocjacji.

## Zobacz także:

1. [Debugging Techniques in Ruby (Techniki debugowania w Ruby)](https://www.toptal.com/ruby/ruby-debugging-techniques)
2. [In-depth look at Ruby's 'puts' (Głębsze spojrzenie na 'puts' w Ruby)](https://ruby-doc.org/core/Kernel.html#method-i-puts)