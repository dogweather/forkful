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

## Co i dlaczego?
Łączenie ciągów znaków jest powszechną operacją w programowaniu. Polega ono na połączeniu dwóch lub więcej ciągów znaków w jeden dłuższy ciąg. Programiści często używają tej operacji, aby łączyć różne teksty i tworzyć bardziej złożone dane.

## Jak to zrobić:
```Haskell
let str1 = "Witaj"

let str2 = "świecie!"

let concatenated = str1 ++ " " ++ str2

print concatenated 
-- Output: Witaj świecie!
```

Kod ten tworzy dwie zmienne przechowujące ciągi znaków, a następnie łączy je w jedną zmienną "concatenated". Wydrukowanie tej zmiennej w konsoli spowoduje wyświetlenie połączonych ciągów znaków.

## Głębsze zanurzenie:
Łączenie ciągów znaków jest popularną operacją w wielu językach programowania, a Haskell nie jest wyjątkiem. Pierwsze funkcje do łączenia ciągów znaków pojawiły się już w języku FORTRAN w 1956 roku. W Haskellu używane są operator ++ lub funkcja concat do łączenia ciągów. Alternatywą dla tej operacji może być również użycie biblioteki Data.Text, która oferuje szybsze i bardziej wydajne operacje na ciągach w porównaniu do standardowych funkcji w Haskellu.

## Zobacz także:
- Dokumentacja Haskell o operacji łączenia ciągów: https://hackage.haskell.org/package/base-4.15.0.0/docs/GHC-Base.html#g:2
- Przykłady użycia biblioteki Data.Text: https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html