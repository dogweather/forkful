---
title:                "Gleam: Tworzenie testów"
programming_language: "Gleam"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego 
Wiele osób zapomina, że pisanie testów jest równie ważne jak pisanie samego kodu. Dzięki testom można upewnić się, że nasze funkcje działają poprawnie i nie powodują błędów. Ponadto, testy pomagają w szybszym wykrywaniu i naprawianiu ewentualnych problemów. W tym wpisie pokażemy, jak napisać testy za pomocą języka Gleam.

## Jak To Zrobić
Najpierw musimy zaimportować "gleam/testing" bibliotekę, która umożliwi nam tworzenie testów. Następnie, możemy rozpocząć pisanie testów za pomocą funkcji "test", która przyjmuje dwa argumenty - nazwę testu i kod do wykonania.

```Gleam
import gleam/testing

test("Check if string is uppercase", fn () {
  assert_eq("GLEAM", String.to_upper("gleam"))
})
```

W powyższym przykładzie, tworzymy test, który sprawdza czy dana funkcja rzeczywiście zmienia tekst na duże litery. Jeśli test zwróci błąd, to znaczy, że nasza funkcja nie działa poprawnie.

Jeśli chcemy przetestować funkcję z niektórymi warunkami, możemy użyć funkcji "test_data", która przyjmuje trzy argumenty - nazwę testu, dane wejściowe oraz kod do wykonania.

```Gleam
import gleam/testing

test_data("Check if string contains given character", [
  ('hello', 'e'),
  ('world', 'r')
], fn (input, expected) {
  assert(input != expected, String.index_of(input, expected))
})
```

W tym przykładzie, testujemy funkcję, która sprawdza czy dany znak znajduje się w tekście. Dzięki temu, możemy przetestować kilka wariantów i upewnić się, że nasza funkcja działa poprawnie.

## Deep Dive
Pisanie testów jest ważnym elementem tworzenia oprogramowania. Dzięki testom, możemy sprawdzić poprawność działania naszych funkcji i szybciej wykrywać ewentualne problemy. W języku Gleam, dostępne są różne funkcje do tworzenia testów, co pozwala na precyzyjne i dokładne testowanie kodu.

## Zobacz również
- [Dokumentacja gleam/testing](https://gleam.run/articles/testing)
- [Przykładowe projekty w języku Gleam](https://github.com/search?q=language%3Agleam)
- [Czym jest język Gleam?](https://gleam.run/)
- [Podstawy programowania w języku Gleam](https://gleam.run/articles/basics)