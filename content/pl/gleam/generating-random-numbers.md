---
title:    "Gleam: Generowanie losowych liczb"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie liczb losowych jest często wykorzystywane do stworzenia różnych gier, symulacji lub w prostych programach do losowania. Jest to również przydatne w testowaniu aplikacji, gdzie wymagane są dane losowe.

## Jak to zrobić

Aby wygenerować liczbę losową w języku programowania Gleam, możemy skorzystać z wbudowanej funkcji `random.int`. Przykładowy kod wyglądałby następująco:

```Gleam
import random

random_int := random.int(1, 10)

// W tym przykładzie, `random_int` będzie liczbą losową z zakresu od 1 do 10.
```

W przypadku, gdy potrzebujemy wygenerować więcej niż jedną losową liczbę, możemy użyć pętli `for` lub funkcji `List.map`.

## Deep Dive

Podczas generowania liczb losowych, istotne jest aby wykorzystać wysokiej jakości algorytm generujący. W języku Gleam, funkcja `random.int` wykorzystuje algorytm Mersenne Twister, który jest szeroko uznawany za jeden z najlepszych algorytmów do generowania liczb losowych.

Jednakże, warto zauważyć, że liczb wygenerowanych przez funkcję `random.int` nie można uważać za całkowicie losowe. Dlatego też, jeśli potrzebujemy bardzo dokładnych danych losowych, powinniśmy skorzystać z biblioteki zewnętrznej, która udostępnia inne, bardziej zaawansowane metody generowania liczb losowych.

## Zobacz także

- Dokumentacja Gleam na temat funkcji `random.int`: https://gleam.run/docs/std-lib/Random.int/
- Wprowadzenie do programowania w języku Gleam: https://gleam.run/getting-started/
- Biblioteka `Erlang Random`, która oferuje szereg funkcji do generowania zaawansowanych danych losowych: https://github.com/erlang/otp/blob/master/lib/stdlib/src/random.erl