---
title:                "Interpolacja ciągu znaków"
html_title:           "C++: Interpolacja ciągu znaków"
simple_title:         "Interpolacja ciągu znaków"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Interpolacja ciągu polega na wstawianiu zmiennych bezpośrednio do tekstu. Programiści robią to, by zwiększyć czytelność kodu i uniknąć powtarzalności.

## Jak to zrobić:

Interpolacji ciągów w Elm nie robi się tak jak w innych językach. W jego przypadku musimy skorzystać z funkcji `++` do łączenia ciągów. Zobacz poniższy przykład:

```Elm
name = "Jan"
hello = "Cześć, " ++ name ++ ", jak się masz?"

-- Wynikiem będzie: "Cześć, Jan, jak się masz?"
```
Interpolację można również zrealizować za pomocą funkcji `String.concat`:

```Elm
name = "Jan"
hello = String.concat ["Cześć, ", name, ", jak się masz?"]

-- Wynikiem będzie: "Cześć, Jan, jak się masz?"
```
## Głębsza wiedza

Elm oferuje bezpieczne łączenie ciągów, różni się od innych języków, gdzie interpolacja ciągów mogłaby prowadzić do podatności na ataki. Interpolację w Elm zaimplementowano w taki sposób, by zapewnić bezpieczeństwo.

Alternatywą dla interpolacji w Elm jest korzystanie z `String.concat` lub funkcji `++`. Wybór między tymi dwoma zależy od preferencji twórcy kodu.

## Zobacz także:

- Dokumentacja Elm (https://elm-lang.org/docs)
- Przewodnik po String.concat (https://package.elm-lang.org/packages/elm/core/latest/String#concat)
- Dyskusja na forum Elm na temat interpolacji ciągów (https://discourse.elm-lang.org/t/string-interpolation-in-elm/638/6)