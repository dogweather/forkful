---
title:    "Elm: Łączenie ciągów znaków"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego?

Czy kiedykolwiek zastanawiałeś się, dlaczego warto połączyć dwie lub więcej ciągów znaków w jedno? W programowaniu, jedną z najczęstszych operacji wykonanych na ciągach jest łączenie ich ze sobą. W tym artykule dowiesz się dlaczego jest to przydatne i jak to zrobić w języku Elm.

## Jak to zrobić?

Aby połączyć dwa ciągi znaków w języku Elm, możesz użyć funkcji `++`. Przykładowy kod wyglądałby następująco:

```Elm
"Hello, " ++ "world!"
```

Wyjściem z tej operacji będzie ciąg znaków "Hello, world!". Możesz również dokonać konkatenacji więcej niż dwóch ciągów:

```Elm
"Programming " ++ "is " ++ "fun!"
```

Powyższy przykład daje wynik "Programming is fun!". Pamiętaj, aby używać tylko ciągów znaków do konkatenacji, inaczej możesz otrzymać błąd.

## Głębszy wgląd

W języku Elm znaki specjalne i liczby mogą być automatycznie przekształcone w ciągi znaków podczas konkatenacji. Na przykład:

```Elm
"Today is " ++ 25 ++ "th of October."
```

Powoduje to wyjście "Today is 25th of October." W przypadku, gdy oba argumenty `++` są ciągami znaków, operator ten po prostu łączy je ze sobą. Jednak jeśli jednym z argumentów jest liczba, Elm automatycznie zamienia ją na ciąg znaków.

## Zobacz również

- [Dokumentacja Elm - konkatenacja ciągów znaków](https://guide.elm-lang.org/strings/concatenation.html)
- [Wideo na temat konkatenacji w języku Elm przez Evan Czaplicki](https://www.youtube.com/watch?v=WmHXPsxEQZY)
- [Artykuł o operacjach na ciągach znaków w języku Elm](https://dev.to/dillion/elm-string-operations-removing-characters-etc-479d)