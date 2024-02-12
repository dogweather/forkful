---
title:                "Łączenie łańcuchów znaków"
aliases:
- /pl/haskell/concatenating-strings.md
date:                  2024-01-20T17:35:01.804020-07:00
model:                 gpt-4-1106-preview
simple_title:         "Łączenie łańcuchów znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

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
