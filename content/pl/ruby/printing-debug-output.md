---
title:                "Ruby: Wydrukowanie komunikatów debugowania"
simple_title:         "Wydrukowanie komunikatów debugowania"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Programowanie to nie tylko pisanie kodu, ale również rozwiązywanie problemów. Aby skutecznie zarządzać swoim kodem i znaleźć ewentualne błędy, ważne jest umiejętne korzystanie z printowania debug output. Pozwala to nam zobaczyć, co dzieje się wewnątrz programu i jakie wartości przyjmuje nasze zmienne. W tym artykule pokażemy Ci, dlaczego warto korzystać z tej techniki.

## Jak to zrobić

Istnieje wiele różnych metod na printowanie debug output w języku Ruby. Najprostszym sposobem jest użycie metody `puts` i podanie jej zmiennej lub ciągu znaków. Przykładowy kod wyglądałby następująco:

```Ruby
x =  5
puts "Wartość zmiennej x: #{x}"
```

Wynikiem będzie wydruk w konsoli: `Wartość zmiennej x: 5`. 

Możemy również wykorzystać metodę `p`, która wypisze nam zawartość zmiennej w sposób bardziej przejrzysty. Przykładowy kod:

```Ruby
fruit = "jabłko"
p "Owoce dostępne w koszyku: #{fruit}"
```

Wynik: `"Owoce dostępne w koszyku: "jabłko"`. 

Jeśli chcemy wyświetlić więcej informacji, możemy wykorzystać metodę `pp`, która wypisze nam zawartość zmiennej w formacie YAML. Przykładowy kod:

```Ruby
require 'pp'

students = ["Anna", "Jan", "Kasia"]
pp "Lista studentów: #{students}"
```

Wynik:

```Ruby
"Lista studentów: [\"Anna\", \"Jan\", \"Kasia\"]"
```

## Głębsze zanurzenie

Debug output jest nie tylko pomocny przy znalezieniu błędów, ale także może być użyteczny w celu zrozumienia, co dzieje się wewnątrz naszego programu. Możemy na przykład wyprintować zawartość zmiennych w różnych momentach wykonania programu, aby zobaczyć, jak zmieniają się ich wartości. Wykorzystując różne metody printowania, możemy zapoznać się z różnymi sposobami prezentacji danych i wybrać ten, który będzie dla nas najbardziej czytelny i przydatny.

## Zobacz również

- [Dlaczego warto używać printowania debug output](https://www.rubyguides.com/2019/01/ruby-debugging/)
- [List of Ruby Debuggers](https://www.rubyguides.com/2019/03/ruby-debuggers/)
- [Jak czytać i rozumieć kod programów w Ruby](https://blog.faelixiablog.pl/jak-czyta%C4%87-i-interpretowa%C4%87-kod-program%C3%B3w-ruby-401a1db327c3)