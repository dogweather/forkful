---
title:                "Ruby: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest niezbędnym elementem wielu programów i aplikacji. Często wykorzystywane jest do symulacji lub testowania pewnych funkcji. Jest to więc niezwykle przydatna umiejętność dla każdego programisty.

## Jak to zrobić

Istnieje wiele sposobów na generowanie losowych liczb w Ruby. Poniżej przedstawione są przykładowe kody, które mogą Ci pomóc w nauce tego zagadnienia.

```ruby
# Wygenerowanie losowej liczby całkowitej
rand(1..10)
# Output: 9

# Losowa liczba zmiennoprzecinkowa z zakresu 0-1
rand()
# Output: 0.236235443

# Wygenerowanie losowego ciągu liczb całkowitych
Array.new(5) { rand(1..10) }
# Output: [2, 4, 8, 5, 9]
```

Możesz także użyć metody `shuffle` do losowego zmiksowania elementów w tablicy lub metody `sample` do wylosowania pojedynczego elementu z tablicy.

## Głębsza wiedza

Generowanie liczb losowych jest oparte na tzw. generatorach pseudolosowych, które wykorzystują określony algorytm do tworzenia ciągu "losowych" liczb. W Ruby używany jest algorytm Mersenne Twister, który jest uważany za jeden z najmocniejszych i najbardziej nieprzewidywalnych generatorów.

Należy pamiętać, że liczby generowane w ten sposób nie są prawdziwie losowe, ponieważ generator musi korzystać z pewnych danych, takich jak tzw. "ziarno" czyli `seed`, aby wygenerować kolejną liczbę. Dlatego ważne jest, aby ustawiać różne wartości ziarna za każdym razem, kiedy chcesz wygenerować nową losową liczbę.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o generowaniu liczb losowych w Ruby, polecam zapoznać się z poniższymi artykułami:

- [Ruby Monks - Random Numbers](https://www.rubymonks.com/generating-random-numbers-in-ruby)
- [Ruby Guides - Random Numbers](https://www.rubyguides.com/2019/01/ruby-random/)
- [Ruby-Doc - Module: Random](https://ruby-doc.org/core-2.7.1/Random.html)

Teraz już wiesz, jak wykorzystać możliwości generowania liczb losowych w Ruby. Bądź kreatywny i używaj ich w swoich projektach, aby uzyskać jeszcze lepsze rezultaty. Powodzenia!