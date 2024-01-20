---
title:                "Zamiana tekstu na wielkie litery"
html_title:           "Gleam: Zamiana tekstu na wielkie litery"
simple_title:         "Zamiana tekstu na wielkie litery"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Zmiana liter na wielkie w ciągu znaków to proces, który przekształca małe litery w duża; na przykład, 'programowanie' staje się 'PROGRAMOWANIE'. Programiści często korzystają z tej techniki, aby podkreślić ważne fragmenty tekstu lub zmienić wygląd napisów.

## Jak to zrobić:

Zobaczmy, jak możemy to zrobić w Gleam:

```Gleam
fn capitalize(text: String) -> String {
  text
  |> String.codepoints
  |> List.map(String.uppercase)
  |> List.reduce("", append)
}

fn main(_) {
  let text = "cześć świecie"
  capitalize(text)
  |> Io.format("{}.n", [text])
}
```

Przykładowe wyjście:

```
CZEŚĆ ŚWIECIE
```

## Na głębszy poziom

Historia i kontekst: Pierwotnie, konwersja liter na wielkie było istotnym elementem starszych systemów komputerowych i języków programowania, które nie obsługiwały małych liter. Chociaż nowoczesne systemy mają o wiele większą elastyczność, wielkie litery nadal są używane do podkreślania i formatowania tekstu.

Alternatywy: Alternatywą dla metody, którą przedstawiliśmy, jest użycie funkcji `String.to_uppercase`. Ten sposób jest bardziej bezpośredni, ale mniej elastyczny, jeśli chcesz dopasować specyficzne znaki lub użyć niestandardowych reguł kapitalizacji.

Szczegóły implementacji: nasz kod najpierw przekształca tekst na listę punktów kodowych, po czym każdy punkt kodowy jest zamieniany na wielką literę. Na koniec, wszystkie te pojedyncze punkty kodu są łączone w jeden ciąg. Dzięki temu metoda jest bardzo skuteczna dla dowolnej długości tekstu, a także obsługuje znaki specjalne i diakrytyczne.