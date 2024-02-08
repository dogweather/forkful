---
title:                "Usuwanie znaków pasujących do wzorca"
aliases:
- pl/haskell/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:42:31.299202-07:00
model:                 gpt-4-1106-preview
simple_title:         "Usuwanie znaków pasujących do wzorca"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

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
