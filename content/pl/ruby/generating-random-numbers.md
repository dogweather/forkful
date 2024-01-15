---
title:                "Generowanie losowych liczb"
html_title:           "Ruby: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest niezbędną umiejętnością w wielu aplikacjach i projektach programistycznych. Jest to przydatne w celu generowania unikalnych identyfikatorów, losowego wybierania elementów z listy lub nawet w symulacjach i testach. W tej krótkiej instrukcji dowiesz się, jak w prosty sposób generować losowe liczby w języku Ruby.

## Jak To Zrobić

```ruby
# Generowanie losowego integera w zakresie od 1 do 10
rand(1..10)
# => 7

# Generowanie losowego floata w zakresie od 0 do 1
rand
# => 0.3514464251832353

# Generowanie losowego integera w zakresie od 1 do 100
(rand * 100).to_i
# => 48
```

Świetnie! Teraz wiedziesz, jak wygenerować losowe liczby w różnych zakresach. Ale co jeśli chcesz mieć większą kontrolę nad generowanymi liczbami? W takiej sytuacji możesz użyć klasy `Random`, która zawiera wiele przydatnych metod do pracy z losowością.

```ruby
# Generowanie losowego integera w zakresie od 100 do 200
random = Random.new
random.rand(100..200)
# => 184

# Generowanie pięciu różnych liczb całkowitych w zakresie od 1 do 10
5.times { puts random.rand(1..10) }
# => 9
# => 2
# => 7
# => 4
# => 6
```

## Deep Dive

Istnieje wiele różnych sposobów generowania losowych liczb w języku Ruby. Jeśli chcesz poznać więcej na ten temat, możesz zajrzeć do dokumentacji języka lub przeczytać o różnych algorytmach generowania liczb losowych. Ważne jest, aby wiedzieć, że niektóre sposoby generowania liczb mogą być bardziej losowe i mniej podatne na powtarzalność niż inne.

## Zobacz również

- [Dokumentacja języka Ruby](https://ruby-doc.org/core-3.0.1/Random.html)
- [Wykorzystanie klasy Random w Ruby](https://medium.com/@katopz/generate-the-random-number-in-ruby-6b591f7798f6)
- [Analiza algorytmów generowania liczb losowych w Ruby](https://spin.atomicobject.com/2017/06/19/random-number-generator-ruby/)