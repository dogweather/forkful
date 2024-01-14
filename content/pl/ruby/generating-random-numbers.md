---
title:                "Ruby: Generowanie losowych liczb"
programming_language: "Ruby"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest kluczowym elementem wielu programów, szczególnie wtedy, gdy potrzebujemy wykonać jakieś losowe działanie lub symulować różne sytuacje. Może to być również użyteczne w tworzeniu gier, kryptografii, czy testowaniu programów. Dzięki temu, że Ruby ma wbudowaną bibliotekę do generowania liczb losowych, możemy to zrobić szybko i łatwo.

## Jak to zrobić

Aby wygenerować losową liczbę, musimy użyć metody `rand`, która przyjmuje jeden lub dwa argumenty. Jeśli podamy dwa argumenty, pierwszy będzie oznaczał początek przedziału, a drugi jego koniec. Należy pamiętać, że liczby te muszą być wyrażone w postaci Float (zmiennoprzecinkowej). Na przykład:

```Ruby
puts rand(0.0..10.0) # wygeneruje liczbę z zakresu od 0 do 10
```

Jeśli nie podamy żadnego argumentu, metoda `rand` wygeneruje liczbę z zakresu od 0 do 1. Możemy również podać tylko jeden argument, wtedy metoda `rand` wygeneruje liczbę z przedziału od 0 do podanej liczby.

```Ruby
puts rand # wygeneruje liczbę z zakresu od 0 do 1
puts rand(50) # wygeneruje liczbę z zakresu od 0 do 50
```

Jeśli chcemy wygenerować liczbę całkowitą, musimy użyć metody `rand` w połączeniu z metodą `to_i`:

```Ruby
puts rand(10).to_i # wygeneruje liczbę całkowitą z zakresu od 0 do 10
```

## Głębsze zagadnienia

Warto wiedzieć, że metoda `rand` używa tzw. generatora liniowego kongruencyjnego (ang. linear congruential generator), który jest algorytmem matematycznym do generowania liczb pseudolosowych. Dlatego wygenerowane przez nią liczby są w rzeczywistości ciągiem matematycznym, a nie zupełnie losowe. W związku z tym, nie powinniśmy polegać na generatorze `rand` w zastosowaniach, gdzie wymagana jest wysoka jakość losowości, takich jak w kryptografii.

Jednym z ciekawszych zastosowań losowych liczb jest tzw. "Monte Carlo method", czyli metoda wykorzystująca liczby pseudolosowe do rozwiązywania problemów matematycznych. Jest to przydatne w przypadkach, gdy nie możemy znaleźć dokładnego rozwiązania przy użyciu tradycyjnych metod.

## Zobacz również

- Oficjalna dokumentacja Ruby na temat generowania liczb losowych: https://ruby-doc.org/core-2.7.1/Random.html
- Przykłady wykorzystania losowych liczb w programowaniu: https://www.rubyguides.com/2018/05/random-numbers-in-ruby/