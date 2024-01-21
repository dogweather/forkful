---
title:                "Generowanie liczb losowych"
date:                  2024-01-20T17:50:17.730155-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generowanie liczb losowych"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Generowanie losowych liczb w Ruby to podstawa wielu aplikacji - od gier po bezpieczeństwo. Używamy tego, aby wprowadzić nieprzewidywalność lub symulować zdarzenia, które naturalnie występują losowo.

## How to: (Jak to zrobić:)
```Ruby
# Sposób 1: Używanie metody rand dla liczb całkowitych
losowa_liczba = rand(100) # Liczba od 0 do 99
puts losowa_liczba

# Sposób 2: Używanie metody rand dla liczb zmiennoprzecinkowych
losowa_liczba_float = rand # Domyślnie od 0.0 do 1.0
puts losowa_liczba_float

# Sposób 3: Wykorzystanie klasy Random dla swojego generatora
generator = Random.new
losowa_liczba_gen = generator.rand(50..100) # Liczba od 50 do 100
puts losowa_liczba_gen
```
Sample output (Przykładowy wynik):
```
42
0.3746322858983456
68
```

## Deep Dive (Głębsze spojrzenie):
Początkowo, generowanie liczb losowych w komputerach nie było takie proste - maszyny nie są przecież naturalnie "losowe". Rozwiązaniem były algorytmy, takie jak liniowy generator kongruentny, które imitowały losowość. Ruby używa Mersenne Twistera - jednego z szybszych i efektywniejszych generatorów.

Istnieją alternatywy jak SecureRandom dla większego bezpieczeństwa, szczególnie ważne w kryptografii. Z kolei metoda `Random.new_seed` generuje nowe ziarno losowości, co jest przydatne, kiedy chcesz mieć inne sekwencje "losowe" za każdym razem.

Warto pamiętać, że te "losowe" liczby nie są całkowicie losowe - są pseudolosowe, bazujące na deterministycznych algorytmach. Dlatego dla zadań krytycznych, takich jak szyfrowanie, lepiej wybrać specjalistyczne, bezpieczne rozwiązania.

## See Also (Zobacz również):
- Ruby-Doc dla Random: [https://ruby-doc.org/core-2.7.0/Random.html](https://ruby-doc.org/core-2.7.0/Random.html)
- Metody SecureRandom: [https://ruby-doc.org/stdlib-2.5.1/libdoc/securerandom/rdoc/SecureRandom.html](https://ruby-doc.org/stdlib-2.5.1/libdoc/securerandom/rdoc/SecureRandom.html)
- Więcej o Mersenne Twisterze: [https://en.wikipedia.org/wiki/Mersenne_Twister](https://en.wikipedia.org/wiki/Mersenne_Twister)