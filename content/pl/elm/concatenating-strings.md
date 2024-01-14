---
title:                "Elm: Łączenie ciągów znaków"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

# Czemu warto łączyć łańcuchy w języku Elm?

Często przy pisaniu oprogramowania potrzebujemy połączyć dwa czy więcej łańcuchów tekstu w jeden. W języku Elm mamy do dyspozycji funkcję `String.concat`, która zadanie to wykonuje w bardzo prosty sposób. W tym artykule pokażę Wam, jak skutecznie korzystać z tej funkcji.

## Jak to zrobić?

W celu połączenia łańcuchów w języku Elm należy skorzystać z funkcji `String.concat`, która przyjmuje jako argumenty dwie listy: pierwszą zawierającą łańcuchy, które chcemy połączyć, oraz drugą zawierającą łańcuchy oddzielające nasze łańcuchy z pierwszej listy.

Przykładowo, jeśli chcemy połączyć łańcuchy "Hello" i "world" oraz oddzielić je spacją, możemy napisać:

```Elm
String.concat ["Hello", "world"] " " -- output: "Hello world"
```

Warto zauważyć, że oddzielający łańcuch jest opcjonalny - jeśli go nie podamy, funkcja automatycznie wykorzysta pusty łańcuch jako separator.

Co więcej, możemy również połączyć więcej niż dwa łańcuchy:

```Elm
String.concat ["Elm", "jest", "super", "!", "!"] " " -- output: "Elm jest super !!"
```

## Głębszy wgląd

Podczas połączenia łańcuchów funkcja `String.concat` wykorzystuje funkcję `String.join`, która dokonuje faktycznego łączenia łańcuchów i oddzielania ich separators. Jest to przydatne szczególnie w przypadku, gdy  nasza pierwsza lista zawiera elementy innego typu niż łańcuchy, ponieważ funkcja `String.join` jest w stanie zamienić je na łańcuchy przed połączeniem.

Warto również zauważyć, że funkcja `String.concat` zwraca nowo utworzony łańcuch, a nie modyfikuje oryginalnych.

## Zobacz również

* Dokumentacja języka Elm o funkcji `String.concat`: https://package.elm-lang.org/packages/elm/core/latest/String#concat
* Dokumentacja języka Elm o funkcji `String.join`: https://package.elm-lang.org/packages/elm/core/latest/String#join