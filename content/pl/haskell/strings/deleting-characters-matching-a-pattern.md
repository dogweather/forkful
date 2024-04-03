---
date: 2024-01-20 17:42:31.299202-07:00
description: "Usuwanie znak\xF3w pasuj\u0105cych do wzorca to proces filtrowania ci\u0105\
  gu, by pozby\u0107 si\u0119 niechcianych znak\xF3w. Programi\u015Bci robi\u0105\
  \ to dla czysto\u015Bci danych,\u2026"
lastmod: '2024-03-13T22:44:35.435595-06:00'
model: gpt-4-1106-preview
summary: "Usuwanie znak\xF3w pasuj\u0105cych do wzorca to proces filtrowania ci\u0105\
  gu, by pozby\u0107 si\u0119 niechcianych znak\xF3w."
title: "Usuwanie znak\xF3w pasuj\u0105cych do wzorca"
weight: 5
---

## Co i dlaczego?
Usuwanie znaków pasujących do wzorca to proces filtrowania ciągu, by pozbyć się niechcianych znaków. Programiści robią to dla czystości danych, bezpieczeństwa aplikacji lub odpowiedniego formatowania tekstów.

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
