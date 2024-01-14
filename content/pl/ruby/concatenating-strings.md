---
title:                "Ruby: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą Ruby, prawdopodobnie spotkałeś się z sytuacją, w której musiałeś połączyć dwa lub więcej ciągów tekstowych w jednym ciągu. Na szczęście w Ruby istnieje łatwy sposób na wykonanie tego zadania: funkcja concatenate, inaczej znana jako "łączenie" lub "sklejanie" ciągów. W tym wpisie dowiesz się, dlaczego warto korzystać z tej operacji oraz jak jej używać.

## Jak to zrobić

Poniżej przedstawiam kilka przykładów wykorzystania funkcji concatenate w Ruby, wraz z wynikami wyświetlonymi pod kodem.

```Ruby
# Przykład 1
puts "Hello" + "World"
# Output: HelloWorld

# Przykład 2
greeting = "Hello"
name = "John"
puts greeting + " " + name
# Output: Hello John

# Przykład 3
numbers = [1, 2, 3]
puts "Numbers: " + numbers.join(",")
# Output: Numbers: 1,2,3
```

## Dogłębne badanie

Funkcja concatenate w Ruby jest nie tylko wygodna, ale również bardzo przydatna w wielu sytuacjach. Pozwala na łączenie nie tylko ciągów tekstowych, ale także innych typów danych, takich jak liczby czy tablice. Możemy również użyć znaku operatora dodawania (+) lub metody "concat" zamiast funkcji concatenate, a wynik będzie taki sam.

Istnieje także możliwość używania interpolacji ciągów, czyli wstawiania zmiennych lub wyrażeń wewnątrz ciągu tekstowego za pomocą znaku "#{}". Przykład:

```Ruby
x = 5
puts "Liczba x wynosi #{x}"
# Output: Liczba x wynosi 5
```

Kolejną przydatną funkcją jest "concat!" - in-place version concatenate, która zmienia oryginalny ciąg tekstowy, zamiast tworzyć nowy. Bardzo ważne jest również określenie dokładnej kolejności ciągów, gdy stosujemy funkcję concatenate, ponieważ może to mieć wpływ na wydajność i skuteczność operacji.

W przypadku większych ciągów tekstowych, lepiej jest używać metody "concat" zamiast znaku +, ponieważ jest bardziej wydajna w przypadku dużej liczby połączonych ciągów.

## Zobacz również

- Dokumentacja Ruby o funkcji concatenate: https://ruby-doc.org/core-2.7.3/String.html#method-i-2B
- Inne metody łączenia ciągów w Ruby: https://apidock.com/ruby/String
- Przydatne wskazówki dotyczące pracy z ciągami w Ruby: https://www.rubyguides.com/ruby-tutorial/string-methods/