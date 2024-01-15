---
title:                "Lączenie ciągów znaków"
html_title:           "Elm: Lączenie ciągów znaków"
simple_title:         "Lączenie ciągów znaków"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Ciągłe łączenie stringów jest kluczowym elementem w procesie tworzenia oprogramowania. W Elm, łączenie stringów jest łatwe i przyjemne, a w tym artykule dowiesz się, jak tego dokonać!

## Jak to zrobić

```Elm
"Hello, " ++ "World" 

```

Kod powyżej łączy dwa stringi "Hello" i "World" w jeden ciąg znaków "Hello, World". Operacja ta jest wykonywana przez operator ++ (plus-plus), który jest specjalnie zaprojektowany do łączenia stringów w Elm. Możemy łączyć więcej niż dwa stringi, wystarczy połączyć je za pomocą kolejnych operatorów ++.

Możemy także łączyć stringi za pomocą funkcji `String.join`, która przyjmuje separator oraz listę stringów jako argumenty. Przykład:

```Elm
String.join ", " ["Hello", "World", "!"] 

```
Wynik powyższego kodu to "Hello, World, !". Funkcja ta jest przydatna, gdy chcemy połączyć wiele stringów zawartych w liście.

## Deep Dive

W Elm, stringi są traktowane jako listy znaków, co oznacza, że posiadają wiele funkcji, które mogą być użyteczne podczas łączenia. Na przykład, funkcja `String.split` dzieli string na dwie części - wszystko przed pierwszym wystąpieniem określonego znaku i wszystko po nim. Jest to przydatne przy manipulowaniu danymi w formie stringów.

Możemy także użyć operatora ++ z wartościami liczb zamiast stringów, wtedy zostaną one automatycznie przekonwertowane na stringi. Przykład:

```Elm
"Age: " ++ 27 

```
Wynik powyższego kodu to "Age: 27". Jest to bardzo użyteczne, gdy chcemy połączyć stringi z wartościami numerycznymi, bez konieczności wcześniejszej konwersji.

## Zobacz też

1. [Dokumentacja operatora ++ w Elm](https://elm-lang.org/docs/syntax#operators)
2. [Funkcja String.join w Elm](https://package.elm-lang.org/packages/elm/core/latest/String#join)
3. [Funkcja String.split w Elm](https://package.elm-lang.org/packages/elm/core/latest/String#split)