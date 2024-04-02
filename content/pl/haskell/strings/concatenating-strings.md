---
date: 2024-01-20 17:35:01.804020-07:00
description: "\u0141\u0105czenie napis\xF3w to po prostu sklejanie ich ko\u0144c\xF3\
  w. Programi\u015Bci robi\u0105 to, aby tworzy\u0107 nowe ci\u0105gi znak\xF3w \u2013\
  \ \u015Bcie\u017Cki do plik\xF3w, wiadomo\u015Bci, dynamiczne teksty\u2026"
lastmod: '2024-03-13T22:44:35.443258-06:00'
model: gpt-4-1106-preview
summary: "\u0141\u0105czenie napis\xF3w to po prostu sklejanie ich ko\u0144c\xF3w.\
  \ Programi\u015Bci robi\u0105 to, aby tworzy\u0107 nowe ci\u0105gi znak\xF3w \u2013\
  \ \u015Bcie\u017Cki do plik\xF3w, wiadomo\u015Bci, dynamiczne teksty\u2026"
title: "\u0141\u0105czenie \u0142a\u0144cuch\xF3w znak\xF3w"
weight: 3
---

## Co i dlaczego?

Łączenie napisów to po prostu sklejanie ich końców. Programiści robią to, aby tworzyć nowe ciągi znaków – ścieżki do plików, wiadomości, dynamiczne teksty w aplikacjach.

## Jak to zrobić:

```Haskell
main :: IO ()
main = do
    let hello = "Witaj, "
    let world = "świecie!"
    putStrLn (hello ++ world)  -- Wykorzystanie operatora ++ do łączenia napisów
    putStrLn $ concat ["Data: ", show (2023, 3, 15)] -- Funkcja concat do łączenia listy napisów
```

Sample output:

```
Witaj, świecie!
Data: (2023,3,15)
```

## Głębsze spojrzenie:

String w Haskell to lista znaków, więc łączenie napisów odbywa się przez konkatenację list. Operator `++` jest standardem od początku języka. Alternatywą może być `concat`, gdy łączymy listę stringów, czy `Data.Text`, biblioteka dla dużych napisów.

Haskell był zaprojektowany z myślą o leniwej ewaluacji, więc połączone napisy nie są od razu tworzone w pamięci, co jest wydajne. Należy jednak pamiętać, że nadużycie `++` może prowadzić do kiepskiej wydajności dla dużych napisów ze względu na konieczność przechodzenia przez całą listę. Tutaj z pomocą przychodzi `Data.Text`, oferując szybszą alternatywę ze stałym czasem dorzucania na końcu.

## Zobacz również:

- [Haskell Documentation on Strings](https://haskell.org/documentation)
- [Hackage: Data.Text library](https://hackage.haskell.org/package/text)
