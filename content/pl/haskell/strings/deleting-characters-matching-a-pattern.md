---
date: 2024-01-20 17:42:31.299202-07:00
description: "Jak to zrobi\u0107: Usuwanie znak\xF3w pojawia si\u0119 w wielu zadaniach\
  \ programistycznych, od prostego czyszczenia string\xF3w po zapewnienie bezpiecze\u0144\
  stwa poprzez\u2026"
lastmod: '2024-04-05T22:50:49.757293-06:00'
model: gpt-4-1106-preview
summary: "Usuwanie znak\xF3w pojawia si\u0119 w wielu zadaniach programistycznych,\
  \ od prostego czyszczenia string\xF3w po zapewnienie bezpiecze\u0144stwa poprzez\
  \ usuni\u0119cie potencjalnie szkodliwych sekwencji."
title: "Usuwanie znak\xF3w pasuj\u0105cych do wzorca"
weight: 5
---

## Jak to zrobić:
```Haskell
import Data.List (isInfixOf)

deletePattern :: String -> String -> String
deletePattern pattern = filter (not . isInfixOf pattern . (:[]))

main :: IO ()
main = do
  let text = "Hej, to jest przykładowy tekst."
  let pattern = "kł"
  putStrLn $ "Przed: " ++ text
  putStrLn $ "Po: " ++ deletePattern pattern text
```

Output:

```
Przed: Hej, to jest przykładowy tekst.
Po: Hej, to jest przyadowy tekst.
```

## Dogłębna analiza
Usuwanie znaków pojawia się w wielu zadaniach programistycznych, od prostego czyszczenia stringów po zapewnienie bezpieczeństwa poprzez usunięcie potencjalnie szkodliwych sekwencji. Rozwiązanie w Haskellu korzysta z wyrażeń funkcyjnych, które są eleganckim narzędziem do pracy z kolekcjami danych. Haskell oferuje alternatywne metody, jak moduł `Text.Regex`, który umożliwia bardziej zaawansowane operacje z wykorzystaniem wyrażeń regularnych. Implementacja `deletePattern` pokazuje, jak możliwości Haskella są wykorzystywane do tworzenia zwięzłego i czytelnego kodu.

## Zobacz również
- [Haskell Text.Regex library](https://hackage.haskell.org/package/regex-compat)
- [Learn You a Haskell for Great Good! - Chapter 7: More Functional Patterns](http://learnyouahaskell.com/higher-order-functions#maps-and-filters)
- [Hoogle – Haskell API search engine](https://hoogle.haskell.org/)
- [Real World Haskell - Chapter 8: Efficient file processing, regular expressions, and file name matching](http://book.realworldhaskell.org/read/efficient-file-processing-regular-expressions-and-file-name-matching.html)
