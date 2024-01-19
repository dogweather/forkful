---
title:                "Konkatenacja ciągów znaków"
html_title:           "Bash: Konkatenacja ciągów znaków"
simple_title:         "Konkatenacja ciągów znaków"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Łączenie napisów, znane jako konkatenacja, to proces łączenia dwóch lub więcej ciągów znaków w jeden ciąg. Programiści robią to, aby manipulować danymi tekstowymi czy tworzyć wiadomości dla użytkowników.

## Jak to zrobić:
W Haskellu, operatorem konkatenacji jest `++`. Wygląda to tak:
```Haskell
main = do
    let a = "Programowanie "
    let b = "w Haskellu jest fajne"
    let c = a ++ b
    putStrLn c
```
Na powyższym przykładzie wydrukuje "Programowanie w Haskellu jest fajne".

## Zanurzmy się głębiej
Konkatenacja w Haskellu jest różna od innych języków programowania. W Haskellu znaki są łączone przy użyciu list. Wynika to z historycznego faktu, że Haskell traktuje ciągi jako listy znaków.

Alternatywą do `++` jest `concat`, które łączy listę napisów w jeden ciąg.

Szczegółem implementacji jest to, że `++` ma złożoność O(n), więc jeśli ciągi są duże, `++` może być wolny. W takich przypadkach `Data.Text` i `Data.ByteString` są lepszymi alternatywami.

## Zobacz także
1. [Dokumenty Hackage dla `++`](http://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:-43--43-)
2. [Haskell Wiki o ciągach](https://wiki.haskell.org/Strings)
3. [Haskell Wiki o Data.Text](https://wiki.haskell.org/Data.Text_Function_cheatsheet)