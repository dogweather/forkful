---
title:                "Ruby: Pisanie do standardowego błędu"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

#Dlaczego pisać do standardowego wyjścia błędów?

Pisanie do standardowego wyjścia błędów jest ważną częścią pisania w języku Ruby. Jest to szczególnie przydatne wtedy, gdy chcemy zobaczyć informacje o błędach lub ostrzeżeniach w naszym kodzie. Umożliwia to łatwiejsze debugowanie i naprawianie potencjalnych błędów.

##Jak to zrobić?

Istnieje kilka sposobów na wypisywanie informacji do standardowego wyjścia błędów w Ruby. Najczęściej używane są metody `warn` i `raise`. Możemy je wywołać za pomocą `Kernel.warn` lub `Kernel.raise`. Przykładowe użycie wygląda następująco:

```Ruby
def divide(x, y)
  if y.zero?
    raise ZeroDivisionError, "Nie można dzielić przez 0!"
  else
    return x / y
  end
end

puts divide(10, 5) # Output: 2
puts divide(10, 0) # Output: ZeroDivisionError: Nie można dzielić przez 0!
```

Jak widać, wywołanie `raise` powoduje zatrzymanie wykonywania kodu i wypisanie informacji o błędzie. Natomiast `warn` pozwala kontynuować działanie programu, ale wypisuje ostrzeżenie.

Inną metodą jest użycie `STDERR.puts`, która bezpośrednio pisze do standardowego wyjścia błędów. Przykładowe użycie:

```Ruby
STDERR.puts "Wystąpił błąd!" # Output: Wystąpił błąd!
```

##Głębsza analiza

Kiedy używamy `raise`, możemy podać nie tylko typ błędu, ale także dodatkową informację. Przykładowo:

```Ruby
def check_age(age)
  if age < 18
    raise StandardError, "Osoba musi być pełnoletnia!"
  else
    puts "Dostęp do witryny udzielony."
  end
end

check_age(16) # Output: StandardError: Osoba musi być pełnoletnia!
```

Także `warn` pozwala na przekazanie dodatkowych parametrów, np. `warn "Błąd przy zapisie pliku!", location: "14:9"`.

#Zobacz też

- Dokumentacja Ruby o wypisywaniu do standardowego wyjścia błędów: https://ruby-doc.org/core-2.7.1/IO.html#method-i-puts
- Poradnik na temat zarządzania błędami w Ruby: https://www.rubyguides.com/2019/05/ruby-error-handling/
- Wideo- tutorial o wypisywaniu do standardowego wyjścia błędów w Ruby: https://www.youtube.com/watch?v=jL-khRbEd2w

Dzięki wypisywaniu do standardowego wyjścia błędów, możemy w prosty sposób znaleźć i naprawić błędy w naszym kodzie. Pamiętajmy o odpowiednim zarządzaniu informacjami oraz wyjątkami, aby nasze programy były jeszcze bardziej wydajne i niezawodne.