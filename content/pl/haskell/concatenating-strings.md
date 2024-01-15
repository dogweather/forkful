---
title:                "Łączenie ciągów znaków"
html_title:           "Haskell: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Wielu programistów, zarówno początkujących jak i doświadczonych, często używa funkcji łączenia stringów w swoim kodzie. Dzięki temu możliwe jest tworzenie czytelnych i spójnych ciągów znaków, co jest niezbędne w wielu aplikacjach. W tym artykule dowiecie się, jak wykorzystać tę funkcjonalność w języku Haskell.

## Jak to zrobić

Aby połączyć dwa stringi w Haskellu, możemy użyć funkcji `++` lub `concat`. Spójrzmy na przykładowy kod:

```Haskell
let name = "John"
let lastName = "Smith"
let fullName = name ++ " " ++ lastName
```

W powyższym przykładzie najpierw tworzymy dwie zmienne przechowujące imię i nazwisko. Następnie, za pomocą funkcji `++`, łączymy je w jedną zmienną `fullName`, która będzie przechowywać pełne imię i nazwisko. 

Możemy również użyć funkcji `concat`, aby połączyć wiele stringów na raz:

```Haskell
let str1 = "Haskell"
let str2 = "jest"
let str3 = "niezwykłym"
let str4 = "językiem"
let sentence = concat [str1, " ", str2, " ", str3, " ", str4]
```

W powyższym przykładzie używamy funkcji `concat`, która przyjmuje jako argument listę stringów, które chcemy połączyć. Z powyższego kodu otrzymamy zmienną `sentence`, która będzie zawierać pełne zdanie: "Haskell jest niezwykłym językiem".

## Rzućmy okiem głębiej

W języku Haskell istnieje również funkcja `<>`, która jest skróconą wersją funkcji `++`. Oba operatory działają w podobny sposób, jednakże różnią się one nieco w działaniu. Funkcja `++` jest bardziej wydajna gdy mamy do czynienia ze sklejaniem większej liczby stringów, a funkcja `<>` jest bardziej wydajna w przypadku łączenia dwóch stringów lub gdy jednym z argumentów jest pusty string.

Ponadto, w języku Haskell możemy wykorzystać również operator `:` w celu sklejenia pojedynczego znaku do istniejącego stringa. Na przykład:

```Haskell
let str = "Haskell"
let newStr = 'i' : str
```

W powyższym przykładzie tworzymy nowy string `newStr`, dodając literę "i" na początku już istniejącego stringa "Haskell". W efekcie otrzymamy "iHaskell".

## Zobacz także

- [Oficjalna dokumentacja Haskell](https://www.haskell.org/documentation/)
- [Tutorial dla początkujących w Haskellu](https://wiki.haskell.org/Tutorials)
- [Inne przykłady zastosowania funkcji `++` i `concat`](https://stackoverflow.com/questions/9613909/how-can-i-concatenate-two-lists-in-haskell)