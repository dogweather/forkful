---
date: 2024-01-20 17:35:01.804020-07:00
description: "Jak to zrobi\u0107: Sample output."
lastmod: '2024-04-05T21:53:36.877037-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u0141\u0105czenie \u0142a\u0144cuch\xF3w znak\xF3w"
weight: 3
---

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
