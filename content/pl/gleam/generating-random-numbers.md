---
title:                "Gleam: Generowanie losowych liczb"
programming_language: "Gleam"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest niezwykle przydatnym narzędziem dla programistów, którzy potrzebują sposobu na generowanie różnych danych do celów testowania lub modelowania. W Gleam istnieje wiele funkcji, które pozwalają na generowanie losowych liczb w prosty i intuicyjny sposób.

## Jak to zrobić

Aby wygenerować losową liczbę w Gleam, możesz użyć funkcji `rand.int`, która pobiera dwa argumenty - minimalną i maksymalną wartość, w obrębie której liczba zostanie wygenerowana. Na przykład, `rand.int(1,10)` wygeneruje losową liczbę całkowitą między 1 a 10. Możesz także użyć funkcji `rand.float` do generowania losowej liczby zmiennoprzecinkowej.

```
Gleam
import rand

fn generate_random_number() {
  let random_int = rand.int(1, 10)
  let random_float = rand.float(0.0, 1.0)
  let output = "Wygenerowano liczby" <> to_string(random_int) <> "i" <> to_string(random_float)
  output
}
```

Wywołanie funkcji `generate_random_number()` doda losową liczbę całkowitą i zmiennoprzecinkową do zmiennej `output`, a następnie ją zwróci.

## Głębszy wgląd

W Gleam istnieje możliwość jeszcze bardziej precyzyjnego generowania losowych liczb. Możesz użyć funkcji `rand.uniform` do generowania liczb zmiennoprzecinkowych w określonym zakresie i z zadana precyzją. Na przykład, `rand.uniform(1.0, 10.0, 0.5)` wygeneruje losową liczbę zmiennoprzecinkową z zakresu od 1.0 do 10.0 z dokładnością do jednego miejsca po przecinku.

## Zobacz także

- Dokumentacja Gleam na temat generowania losowych liczb: https://gleam.run/core/random
- Przykładowy projekt Gleam z wykorzystaniem funkcji `rand`: https://github.com/gleam-lang/gleam-by-example/blob/master/src/random.gleam