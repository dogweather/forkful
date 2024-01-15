---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "Haskell: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami w naszych programach chcemy usunąć znaki, które pasują do określonego wzorca. Może to być przydatne, gdy pracujemy z plikami tekstowymi lub analizujemy dane.

## Jak to zrobić

Możemy użyć funkcji `filter` w języku Haskell, aby wybrać tylko te znaki, które nie pasują do naszego wzorca. Następnie możemy połączyć je ponownie za pomocą funkcji `concat`. Przykładowy kod wyglądałby następująco:

```Haskell
deleteMatching :: String -> String -> String
deleteMatching pattern = concat . filter (\c -> not (c `elem` pattern))
```

Aby użyć tej funkcji, wystarczy przekazać jej wzorzec oraz napis, z którego chcemy usunąć pasujące znaki. Na przykład, jeśli chcemy usunąć wszystkie spacje z napisu "Hello World", możemy użyć następującego wyrażenia: `deleteMatching " " "Hello World"`. Wynik wyglądałby następująco: "HelloWorld".

## Dogłębna analiza

Funkcja `filter` działa w ten sposób, że przyjmuje listę i zwraca tylko te elementy, które spełniają określony warunek. W naszym przypadku, warunkiem jest to, że dany znak nie występuje w naszym wzorcu. Następnie, funkcja `concat` po prostu łączy wszystkie znaki w jeden napis.

Warto również wspomnieć o funkcji `elem`, która sprawdza, czy dany element znajduje się w liście. Dzięki temu możemy wykluczyć wszystkie znaki, które zawierają się w naszym wzorcu.

## Zobacz także

- [Dokumentacja funkcji `filter` w Haskellu](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:filter)
- [Dokumentacja funkcji `concat` w Haskellu](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:concat)
- [Inne przydatne funkcje w Haskellu](https://wiki.haskell.org/Cheatsheet)