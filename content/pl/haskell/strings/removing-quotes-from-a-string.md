---
date: 2024-01-26 03:39:46.648636-07:00
description: "Jak to zrobi\u0107: W Haskellu mo\u017Cemy stworzy\u0107 funkcj\u0119\
  , kt\xF3ra usunie wszystkie cudzys\u0142owy z danego ci\u0105gu znak\xF3w. To jakby\
  \ powiedzie\u0107 cudzys\u0142owom, \u017Ceby spad\u0142y,\u2026"
lastmod: '2024-03-13T22:44:35.439327-06:00'
model: gpt-4-0125-preview
summary: "W Haskellu mo\u017Cemy stworzy\u0107 funkcj\u0119, kt\xF3ra usunie wszystkie\
  \ cudzys\u0142owy z danego ci\u0105gu znak\xF3w."
title: "Usuwanie cudzys\u0142ow\xF3w z ci\u0105gu znak\xF3w"
weight: 9
---

## Jak to zrobić:
W Haskellu możemy stworzyć funkcję, która usunie wszystkie cudzysłowy z danego ciągu znaków. To jakby powiedzieć cudzysłowom, żeby spadły, i upewnić się, że zrozumieli aluzję.

```Haskell
import Data.List (intercalate)
import Data.Char (isPunctuation)

removeQuotes :: String -> String
removeQuotes = filter (\c -> c /= '"' && c /= '\'')

main :: IO ()
main = do
    let stringWithQuotes = "Haskell powiedział, \"Nauczmy się kilku funkcji!\""
    putStrLn $ removeQuotes stringWithQuotes
```

Przykładowe wyjście:

```
Haskell powiedział, Nauczmy się kilku funkcji!
```

## Dogłębniej
Dawno, dawno temu, zanim ciągi znaków w programowaniu stały się równie powszechne jak filmy z kotami w internecie, manipulacja tekstem była karkołomnym zajęciem. Ale wraz z ewolucją języków programowania, ciągi znaków stały się kluczową częścią kodowania. Jednakże, cudzysłowy pozostały mieczem obosiecznym—niezbędne do definiowania ciągów, ale uciążliwe, gdy są zawarte jako rzeczywiste dane.

Alternatywy? Zamiast odpędzać wszystkie cudzysłowy jak muchy, możesz być bardziej selektywny. Możesz chcieć usunąć tylko najbardziej zewnętrzne cudzysłowy (klasyczne przycięcie) lub obsłużyć cudzysłowy uciekające wewnątrz ciągu.

Jeśli chodzi o implementację, funkcja `removeQuotes` wyżej używa lambdy do sprawdzenia każdego znaku (`c`), aby zobaczyć, czy jest uciążliwym cudzysłowem i odpowiednio go filtrowania. To prostolinijne podejście, ale dla większych tekstów lub bardziej złożonych zasad, warto rozważyć biblioteki parserów takie jak `Parsec`, które mogą dać więcej finezji i mocy w przetwarzaniu tekstu.

## Zobacz również:
- Dla miłośników regex: [Text.Regex.Posix](https://hackage.haskell.org/package/regex-posix)
- Przyjazne wprowadzenie do ciągów znaków w Haskellu: [Learn You a Haskell for Great Good! - Zacznijmy](http://learnyouahaskell.com/starting-out#strings)
