---
title:                "Pisanie testów"
html_title:           "Gleam: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego pisać testy w Gleam?

Pisanie testów to ważna część procesu tworzenia oprogramowania. W Gleam możemy pisać testy szybko i łatwo, co pozwala nam na sprawdzenie poprawności kodu oraz uniknięcie błędów w przyszłości.

## Jak pisać testy w Gleam?

Pierwszym krokiem jest zainicjowanie testów za pomocą modułu `gleam/test`. Następnie tworzymy funkcję testową z pomocą makra `test/2`. Przykładowy kod wygląda następująco:

```Gleam
import gleam/test

test "Dodawanie liczb" {
  assert.equal 2 (1 + 1)
  assert.equal 3 (1 + 2)
}
```

Po uruchomieniu testów za pomocą polecenia `gleam test` powinniśmy otrzymać następujący wynik:

```Shell
✓ Dodawanie liczb
```

W przypadku, gdy testy zawierają błędy, otrzymamy informację o nich wraz z informacją, na której linii kodu wystąpił problem.

## Głębszy zanurzenie w testach w Gleam

W Gleam możemy pisać testy dla modułów, funkcji oraz typów danych. Możemy także korzystać z metod takich jak `assert.equl`, `assert.true` lub `assert.false` w celu sprawdzenia zachowania naszego kodu.

Dodatkowo, możemy tworzyć testy oparte na warunkach za pomocą makra `test/4`, które pozwala nam na testowanie różnych wariantów danych wejściowych i oczekiwanych wyników.

## Zobacz także

- Dokumentacja Gleam: https://gleam.run/documentation
- Oficjalne repozytorium Gleam: https://github.com/gleam-lang/gleam