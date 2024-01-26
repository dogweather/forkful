---
title:                "Obsługa błędów"
date:                  2024-01-26T00:52:23.889386-07:00
model:                 gpt-4-1106-preview
simple_title:         "Obsługa błędów"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/handling-errors.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Obsługa błędów polega na przewidywaniu sytuacji, kiedy coś może pójść nie tak w kodzie i zarządzaniu tymi przypadkami w sposób łagodny. Programiści robią to, ponieważ sprawia to, że aplikacje są solidne i przyjazne dla użytkownika, nawet w przypadku nieoczekiwanych problemów.

## Jak to zrobić:
W Gleam często będziesz korzystać z typu `Result` do obsługi błędów. Jest to wyliczenie z dwoma wariantami: `Ok` (dla sukcesu) i `Error` (dla porażki). Oto prosty przykład:

```Gleam
pub fn might_fail(break_it: Bool) -> Result(Int, String) {
  if break_it {
    Error("Oops! It broke.".to_string())
  } else {
    Ok(42)
  }
}

pub fn main() {
  let result = might_fail(False)
  case result {
    Ok(value) => value
    Error(message) => {
      io.println(message)
      0
    } 
  }
}
```

Jeśli uruchomisz `main` z `might_fail(False)`, zwróci `42`. Jeśli przekażesz `True`, wyświetli "Oops! It broke." i zwróci `0`.

## Glebokie zanurzenie
Podejście Gleam do obsługi błędów jest inspirowane jego korzeniami w Erlangu. Tradycyjnie, Erlang stosuje filozofię "niech się to zawiesi" ("let it crash"), polegając na drzewach nadzoru do zarządzania awariami procesów. Jednakże, kiedy piszesz kod Gleam, który nie jest wewnątrz procesu przeznaczonego do nadzoru, jak na przykład w funkcji biblioteki, chciałbyś jawnie obsługiwać błędy.

Alternatywy dla używania `Result` obejmują użycie typu `Option` w przypadkach, gdy coś może być `None` (nic) lub `Some` (coś), ale te nie przenoszą informacji o błędach. Do sygnalizowania błędów przez granice procesów możesz używać mechanizmów przekazywania wiadomości w Erlangu.

Obsługa błędów w Gleam odzwierciedla styl programowania funkcyjnego, gdzie efekty uboczne (jak błędy) są zarządzane za pomocą typów i dopasowania wzorców, zapewniając jasność i przewidywalność w zarządzaniu błędami.

## Zobacz również
- [Erlang's Error Handling](http://erlang.org/doc/reference_manual/errors.html)