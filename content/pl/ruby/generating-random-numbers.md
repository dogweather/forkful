---
title:                "Tworzenie losowych liczb"
html_title:           "Ruby: Tworzenie losowych liczb"
simple_title:         "Tworzenie losowych liczb"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Dlaczego & PoCo?

Generowanie losowych liczb jest jednym z podstawowych zadań programistów. Pozwala to na tworzenie różnych symulacji, gier, testów i wielu innych rzeczy. Innymi słowy, jest to niezbędne narzędzie w zdecydowanej większości projektów informatycznych.

# Jak to zrobić:

W Ruby istnieje wiele różnych sposobów na generowanie losowych liczb. Jednym z nich jest użycie metody `rand` z biblioteki `Kernel`. Oto przykład:

```ruby
puts rand(10)	# wygenerowanie losowej liczby od 0 do 10
```

Wynik może wyglądać na przykład tak: `7`.

Jeśli chcemy wygenerować wiele liczb naraz, możemy skorzystać z metody `times` w połączeniu z `rand`:

```ruby
5.times { puts rand(100) }	# wygenerowanie 5 losowych liczb od 0 do 100
```

Możemy również wygenerować liczbę całkowitą z zakresu przy użyciu metody `rand` z podaniem dwóch argumentów, czyli początku i końca zakresu:

```ruby
puts rand(1..100)	# wygenerowanie losowej liczby całkowitej od 1 do 100
```

# Głębszy wgląd:

Generowanie losowych liczb jest jednym z najstarszych zadań programistów. Już w latach 50. XX wieku istniały różne algorytmy, które miały na celu generowanie liczb pseudolosowych. Jednym z najbardziej popularnych jest metoda LCG (Linear Congruential Generator). Obecnie jednak istnieje wiele innych, bardziej skomplikowanych i bezpieczniejszych sposobów na generowanie losowych liczb.

W Ruby istnieją również inne biblioteki do generowania liczb losowych, takie jak `SecureRandom`, która zapewnia większe bezpieczeństwo podczas generowania liczb pseudolosowych. Istnieją również biblioteki dedykowane do generowania konkretnych typów liczb, na przykład `RandomInteger` do generowania liczb całkowitych.

# Zobacz również:

- [Ruby Dokumentacja - Metoda `rand`](https://ruby-doc.org/core-3.0.2/Kernel.html#method-i-rand)
- [Ruby Dokumentacja - Biblioteka `SecureRandom`](https://ruby-doc.org/stdlib-3.0.2/libdoc/securerandom/rdoc/SecureRandom.html)
- [RandomInteger gem](https://github.com/mayerdan/random_integer)