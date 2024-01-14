---
title:    "Ruby: Tworzenie losowych liczb"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest nieodłączną częścią programowania w wielu językach, w tym w Rubim. Pozwala ono tworzyć różnorodne symulacje, gry, testy oraz wiele innych praktycznych zastosowań. Dodatkowo, jest to również świetny sposób na naukę i eksperymentowanie z różnymi algorytmami.

## Jak to zrobić

W Rubim, generowanie losowych liczb jest bardzo proste i niewymagające dużego nakładu pracy. Wystarczy użyć wbudowanej metody ```rand``` i podać zakres, z którego chcemy wygenerować liczbę. Na przykład, jeśli chcemy wylosować liczbę od 1 do 10, użyjemy poniższego kodu:

```Ruby
rand(1..10)
```

Jeśli chcemy wylosować liczbę całkowitą z zakresu, można użyć metody ```Integer``` i podać zakres jako argument. Na przykład:

```Ruby
Integer(rand(1..10))
```

Możemy również podać zakres liczb zmiennoprzecinkowych lub nawet wylosować losowy element z tablicy. Istnieje wiele innych opcji do wyboru, dlatego warto eksperymentować i poznawać różne metody dostępne w Rubim.

## Głębsza analiza

W Rubim, metoda ```rand``` korzysta z generatora liczb pseudolosowych, który jest zasilany przez wartość zwrotną metody ```seed``` (domyślnie ustawioną na aktualny czas). Jest to ważne, ponieważ jeśli w tym samym momencie uruchomimy nasz kod z wykorzystaniem ```rand```, otrzymamy ten sam wynik. Możemy sami zdefiniować wartość ```seed``` za pomocą metody ```srand```, co pozwala na otrzymanie różnych wyników przy każdym uruchomieniu programu.

Zwróćmy również uwagę na to, że pomimo użycia metody ```rand``` z podaniem konkretnego zakresu, wynik może się różnić. Jest to spowodowane faktem, że Rubi zaokrągla liczby zmiennoprzecinkowe do pewnej dokładności, co wpływa na końcowy wynik.

## Zobacz również

- [Oficjalna dokumentacja Ruby o generowaniu liczb losowych](https://ruby-doc.org/core-3.0.0/Random.html)
- [Przykładowa gra napisana w Rubim wykorzystująca metody do generowania losowych liczb](https://github.com/luminousrubyist/number-guessing-game/blob/main/main.rb)
- [Wyjaśnienie pojęcia generatora liczb pseudolosowych](https://www.geeksforgeeks.org/pseudorandom-number-generator-prng/)