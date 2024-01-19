---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "C: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Usuwanie znaków zgodnie z wzorcem to proces, w którym z danych tekstowych usuwane są wszystkie znaki, pasujące do określonego wzoru. Programiści korzystają z tego podczas czyszczenia, normalizacji czy analizy danych, umożliwiając precyzyjną manipulację na danych tekstowych.

## Jak to zrobić:

Przykład kodu Elm, który usuwa wszystkie wystąpienia określonego znaku z ciągu:

```Elm
import String

usunZnaki : Char -> String -> String
usunZnaki znak tekscik =
    String.split (String.fromChar znak) tekscik
        |> String.join ""

main =
    usunZnaki 'a' "banana"
```

Wynik:
```
"bnn"
```

## Głębsze spojrzenie:

Usuwanie znaków zgodnie z wzorcem jest techniką używaną w przetwarzaniu języka naturalnego od czasów wczesnych komputerów i jest nadal podstawą wielu współczesnych operacji na tekstach. Alternatywą dla tej techniki może być użycie regularnych wyrażeń, które są dla niektórych języków jak Python lub JavaScript bardziej naturalnym sposobem manipulowania danymi tekstowymi. W Elm możemy także zastosować funkcję `String.filter`, która jest jednak mniej efektywna, gdyż przechodzi przez każdy znak ciągu niezależnie od tego, czy pasuje do wzorca.

## Zobacz również:

Jeśli chcesz dowiedzieć się więcej na temat operacji na ciągach w Elm, zapraszam do odwiedzenia oficjalnej dokumentacji:
[String](https://package.elm-lang.org/packages/elm/core/latest/String)  

Więcej informacji na temat pracy z tekstem w języku programowania Elm można znaleźć tutaj:
[Working with strings in Elm](https://elmprogramming.com/strings.html)