---
title:    "Gleam: Generowanie losowych liczb"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest niezwykle przydatną umiejętnością w programowaniu. Może być wykorzystywane do stworzenia symulacji, losowych eventów lub nawet do generowania unikalnych kodów dla użytkowników.

## Jak to zrobić

Aby wygenerować losową liczbę w Gleam, możemy użyć funkcji `random.int()` z modułu `Random`. Możesz przekazać zakres liczb, które chcesz wygenerować jako parametry do funkcji. Na przykład, aby wygenerować liczbę z zakresu od 1 do 10, użyjemy kodu:

```Gleam
let number = Random.int(1, 10)
```

Możemy także wygenerować listę losowych liczb za pomocą funkcji `random.ints()`. Przykładowy kod poniżej wygeneruje 5 losowych liczb od 1 do 100:

```Gleam
let numbers = Random.ints(5, 1, 100)
```

Bez ustalenia zakresu, funkcja `Random.int()` będzie generować losowe liczby całkowite z zakresu od 0 do `MAX_SAFE_INTEGER`.

## Głębszy wgląd

W przypadku gdy potrzebujemy większej kontroli nad generowaniem liczb, możemy użyć sekwencji `Random.Generator`. Ta sekwencja pozwala na ustawienie nasionka (ang. seed) oraz na definiowanie własnego sposobu generowania losowych liczb.

Przykładowy kod poniżej wykorzystuje `Random.Generator` do wygenerowania listy losowych liczb z przedziału od 0 do 1:

```Gleam
let random_generator = Random.make_generator()
let numbers = Random.Generator.floats(random_generator, 0.0, 1.0)
```

Możemy także ustawić własną funkcję generującą, np. z wykorzystaniem innych funkcji z modułu `Random`, takich jak `string()`, `bool()`, lub `char()`.

## Zobacz także

- Oficjalna dokumentacja modułu Random w Gleam: https://gleam.run/documentation/standard-libraries/random/
- Poradnik dotyczący generowania losowych liczb w Gleam: https://blog.gleam.run/random-numbers/