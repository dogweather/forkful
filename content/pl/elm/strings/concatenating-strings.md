---
date: 2024-01-20 17:34:32.058679-07:00
description: "Jak to zrobi\u0107: W Elm, mo\u017Cemy u\u017Cy\u0107 operatora `++`,\
  \ \u017Ceby po\u0142\u0105czy\u0107 ci\u0105gi znak\xF3w. Oto jak to wygl\u0105\
  da w praktyce."
lastmod: '2024-03-13T22:44:35.313245-06:00'
model: gpt-4-1106-preview
summary: "W Elm, mo\u017Cemy u\u017Cy\u0107 operatora `++`, \u017Ceby po\u0142\u0105\
  czy\u0107 ci\u0105gi znak\xF3w."
title: "\u0141\u0105czenie \u0142a\u0144cuch\xF3w znak\xF3w"
weight: 3
---

## Jak to zrobić:
W Elm, możemy użyć operatora `++`, żeby połączyć ciągi znaków. Oto jak to wygląda w praktyce:

```Elm
hello : String
hello = "Cześć, "

name : String
name = "Jan!"

greeting : String
greeting = hello ++ name  -- "Cześć, Jan!"

main = 
    text greeting
```

Wynik działania:
```
Cześć, Jan!
```

## W głębinie:
Łączenie ciągów znaków jest podstawową funkcją w wielu językach programowania i istnieje od początków informatyki. W Elm, podobnie jak w innych funkcyjnych językach, skupiamy się na niemutowalności – raz stworzone ciągi nie zmieniają swojego stanu, a łączenie tworzy nowy ciąg.

Alternatywą dla operatora `++` jest użycie funkcji `String.concat`, która bierze listę ciągów i scala je w jeden. Oto przykład:

```Elm
greeting : String
greeting = String.concat [hello, name]

main = 
    text greeting
```

Gdy chodzi o implementację, Elm, który kompiluje się do JavaScript, musi efektywnie zarządzać pamięcią. Operator `++` i funkcja `String.concat` są zoptymalizowane, by minimalizować narzut związany z tworzeniem nowych stringów.

## Zobacz również:
- [Elm string concatenation](https://package.elm-lang.org/packages/elm/core/latest/String#concat) dokumentacja funkcji `String.concat`.
- [Elm core String module](https://package.elm-lang.org/packages/elm/core/latest/String) dla kompletnego zestawu funkcji operujących na stringach.
