---
title:                "Wydrukowanie wyników debugowania"
html_title:           "Gleam: Wydrukowanie wyników debugowania"
simple_title:         "Wydrukowanie wyników debugowania"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Czym jest wyjście debugowania i dlaczego programiści go używają?

Wyjście debugowania to narzędzie, które pomaga programistom w śledzeniu i zrozumieniu kodu, szczególnie w celu naprawiania błędów. Pozwala ono na wyświetlenie informacji podczas wykonywania programu, co ułatwia identyfikację problemów oraz zrozumienie, jak kod działa.

## Jak to zrobić?

Możesz użyć funkcji `debug!`, aby wyświetlić dane lub zmienne do konsoli podczas wykonywania kodu. Na przykład:

```Gleam
fn print_example() {
  let name = "John"
  let age = 30
  debug!("Hello", name)
  debug!("He is", age, "years old")
}
```

Wyżej wymieniony kod wyświetli w konsoli tekst `Hello John` oraz `He is 30 years old` podczas wykonania programu.

## W pogłębionej perspektywie

Wyjście debugowania jest popularną techniką, która jest stosowana przez programistów od wielu lat. Alternatywnym sposobem na wyświetlanie informacji jest użycie specjalnych narzędzi do debugowania, takich jak debugery. Jednakże, w wielu przypadkach użycie `debug!` jest szybszym i prostszym rozwiązaniem.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o wyjściu debugowania w języku Gleam, możesz przejrzeć dokumentację lub przeczytać artykuły na ten temat. Możesz także sprawdzić alternatywne metody wyświetlania informacji, aby znaleźć najlepsze rozwiązanie dla swoich potrzeb.