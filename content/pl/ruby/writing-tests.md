---
title:                "Ruby: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego

Testowanie jest nieodłączną częścią procesu pisania kodu. Zapewnia ono pewność, że nasza aplikacja działa poprawnie i minimizuje ryzyko wystąpienia błędów. Dzięki testom nie tylko sprawdzamy czy nasz kod działa poprawnie teraz, ale również ułatwiamy sobie pracę w przyszłości - przy wprowadzaniu zmian i rozwijaniu aplikacji.

## Jak to zrobić

W celu napisania testów w Ruby możemy skorzystać z kilku bibliotek, takich jak Minitest czy RSpec. Poniżej pokazane są przykłady testów przy użyciu Minitest.

#### Test jednostkowy

```Ruby
require 'minitest/autorun'

class Calculator
  def add(a, b)
    a + b
  end
end

class TestCalculator < Minitest::Test
  def setup
    @calculator = Calculator.new
  end

  def test_addition
    result = @calculator.add(2, 3)
    assert_equal 5, result
  end
end
```

W powyższym przykładzie tworzymy klasę Calculator, która ma metodę add, która dodaje dwie liczby. W klasie testowej tworzymy obiekt tej klasy i w danym teście sprawdzamy, czy wynik dodawania jest poprawny.

#### Test akceptacyjny

```Ruby
require 'minitest/autorun'

class Calculator
  def divide(a, b)
    a / b
  end
end

class TestCalculator < Minitest::Test
  def setup
    @calculator = Calculator.new
  end

  def test_division_by_zero
    assert_raises ZeroDivisionError do
      @calculator.divide(2, 0)
    end
  end
end
```

W tym przypadku testujemy zachowanie aplikacji w przypadku podziału przez zero. Oczekujemy, że metoda rzuci wyjątek ZeroDivisionError.

## Głębszy wykład

Testy są nie tylko przydatne w celu sprawdzenia poprawności kodu, ale także pozwalają na kontrolę jakości aplikacji. Dobrą praktyką jest pisanie testów przed napisaniem kodu, co pomaga w zdefiniowaniu oczekiwanej funkcjonalności oraz struktury kodu.

Istotne jest również pokrycie testami jak największej ilości scenariuszy - zarówno poprawnych, jak i błędnych. Dzięki temu mamy pewność, że nasza aplikacja jest niezawodna i odporna na różne przypadki.

Warto również pamiętać, że testy nie są jednorazową czynnością - powinniśmy aktualizować je przy każdej zmianie w kodzie, aby zapewnić, że nasza aplikacja nadal działa poprawnie.

## Zobacz również

- [Dlaczego warto testować w Ruby?](https://www.rubyguides.com/2015/01/rspec-tutorial/)
- [Przewodnik po testowaniu w Ruby](https://www.rubyguides.com/2018/12/ruby-testing-guide/)
- [Oficjalna dokumentacja Minitest](https://ruby-doc.org/stdlib-2.5.0/libdoc/minitest/rdoc/MiniTest.html)
- [Oficjalna dokumentacja RSpec](http://rspec.info/documentation/)