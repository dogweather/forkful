---
title:    "Gleam: Wyświetlanie informacji debugowania"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Drukowanie informacji debugujących jest częstym narzędziem wykorzystywanym przez programistów do śledzenia i rozwiązywania problemów w kodzie. Pozwala to na wyświetlanie wartości zmiennych, wywoływania funkcji oraz informacji o błędach, co znacznie ułatwia proces debugowania.

## Jak to zrobić

Aby wydrukować informacje debugujące w Gleam, należy użyć funkcji `debug.log()` oraz przekazać do niej wartość, którą chcemy wyświetlić jako argument. Przykładowy kod wyglądałby następująco:

```Gleam
let name = "John"

debug.log(name) // wydrukuje "John" 
```

Dodatkowo, można również wyświetlać informacje o kodzie w formie tzw. "śladu wywołań" (ang. stack trace), co pozwala na dokładniejsze prześledzenie drogi wywołań funkcji. Aby to zrobić, należy użyć funkcji `debug.stack_trace()`. Poniżej przedstawiamy przykład jej użycia:

```Gleam
fn greet(name) {
  debug.log("Witaj, " ++ name ++ "!") // wydrukuje "Witaj, John!"
}

fn main() {
  let name = "John"

  greet(name)

  debug.stack_trace() // wyświetli ślad wywołań: "main() -> greet(name)"
}
```

## Głębszy zanurzenie

Wyświetlanie informacji debugujących jest nie tylko przydatne w procesie rozwiązywania problemów, ale również pozwala na lepsze zrozumienie działania programu. Możemy poprzez wyświetlane wartości zmiennych obserwować zmiany w ich wartościach podczas wykonywania kodu oraz prześledzić kolejność wywołań funkcji.

## Zobacz też

- Dokumentacja Gleam: https://gleam.run/
- Przewodnik po Glemie dla początkujących: https://gleam.run/book/
- Wprowadzenie do programowania funkcyjnego w Gleam: https://gleam.run/docs/functional-programming