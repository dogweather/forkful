---
title:                "Generowanie losowych liczb"
html_title:           "Elm: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie liczb losowych jest jednym z najważniejszych narzędzi w programowaniu, które pozwalają na wykorzystanie losowych danych w tworzeniu aplikacji, gier czy symulacji. W przypadku języka Elm, generowanie liczb losowych jest nie tylko proste, ale także niezwykle efektywne.

## Jak to zrobić

Aby wygenerować liczbę losową w Elm, należy użyć wbudowanej funkcji `random` oraz modułu `Random`. Poniżej znajduje się przykładowy kod:

```Elm
import Random exposing (..)

-- Generowanie liczby z przedziału od 1 do 10
randomInt = 
    random 1 10 int

-- Generowanie liczby zmiennoprzecinkowej z przedziału od 0 do 1
randomFloat = 
    random 0 1 float

-- Generowanie losowej wartości typu String
randomString = 
    randomString "Ala ma kota" string
```

W powyższym przykładzie `random` przyjmuje dwa argumenty – minimalną i maksymalną wartość, a także określa typ wartości, który ma zostać wygenerowany. Dzięki temu w łatwy sposób możemy wygenerować dowolną liczbę lub nawet ciąg znaków.

## Zagłębianie się

W przypadku generowania liczb losowych, ważne jest, aby pamiętać o tym, że są one wybierane z ustalonego zakresu. Jeśli więc chcemy mieć większą kontrolę nad generowanymi wartościami, możemy zastosować funkcję `map` oraz `andThen`. Pozwalają one na dostosowanie wyniku w zależności od wybranej przez nas funkcji.

```Elm
-- Generowanie losowej wartości z przedziału od 1 do 100, 
-- ale tylko w przypadku gdy jest ona parzysta
randomEvenNumber = 
    random 1 100 int
        |> map (\x -> x * 2)
        |> andThen (\x -> if x % 2 == 0 then Ok x else randomEvenNumber)
```

W powyższym przykładzie używamy `map` do wygenerowania liczby parzystej oraz `andThen` do sprawdzenia, czy wygenerowana liczba spełnia nasze wymagania. Jeśli nie, to wywołujemy funkcję `randomEvenNumber` ponownie, aż do momentu, gdy zostanie wygenerowana liczba spełniająca nasze kryteria.

## Zobacz także

- Oficjalna dokumentacja: https://elm-lang.org/docs/random
- Przykłady z wykorzystaniem modułu Random: https://elmprogramming.com/random-numbers.html
- Grywalizacja aplikacji z wykorzystaniem liczb losowych: https://medium.com/@ckirkwood/adding-gamification-to-elm-apps-with-randomness-d0562bad4df2