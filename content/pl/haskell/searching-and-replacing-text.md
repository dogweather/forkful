---
title:                "Wyszukiwanie i zamiana tekstu"
html_title:           "Haskell: Wyszukiwanie i zamiana tekstu"
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Dlaczego

Dlaczego ktokolwiek zajmowałby się wyszukiwaniem i zamianą tekstu? Dzieje się tak, ponieważ często musimy zmieniać określone części tekstu w naszych programach lub dokumentach. Zamiast ręcznie edytować każde wystąpienie tekstu, możemy użyć narzędzi programistycznych, takich jak Haskell, aby wykonać to zadanie automatycznie i szybko.

# Jak to zrobić

Aby wyszukiwać i zamieniać tekst w Haskellu, musimy użyć wbudowanej funkcji `replace`, która przyjmuje trzy argumenty: łańcuch wejściowy, wyszukiwany wyraz i wyraz zastępczy. Przykładowy kod wyglądałby następująco:

```Haskell
-- Zastąpienie każdego wystąpienia wyrazu "kot" wyrazem "pies"
replace "Miałam kota, ale uciekł." "kot" "pies" 
-- Output: "Miałam psa, ale uciekł."
```

Możemy również użyć funkcji wyszukującej `isInfixOf`, aby sprawdzić, czy dany wyraz znajduje się w tekście, a następnie wykorzystać tę informację do zmiany tekstu.

```Haskell
-- Sprawdzenie czy wyraz "czekolada" znajduje się w tekście
isInfixOf "Lubię jeść czekoladę." "czekolada" 
-- Output: True

-- Zamiana wszystkich wystąpień wyrazu "czekolada" wyrazem "owoce"
replace "Lubię jeść czekoladę." "czekolada" "owoce" 
-- Output: "Lubię jeść owoce."
```

# Głębsze wgląd

W Haskellu istnieje wiele funkcji i bibliotek, które umożliwiają bardziej zaawansowane wyszukiwanie i zamianę tekstu. Na przykład, jeśli chcemy zmienić tylko część tekstu, możemy użyć funkcji `splitOn`, aby podzielić tekst na części przed i po wyrazie, który chcemy zamienić. 

Możemy również użyć biblioteki "regex", która pozwala na wyrażenia regularne, aby wykonać bardziej skomplikowane wyszukiwanie i zamianę tekstu.

# Zobacz również

- [Dokumentacja Haskell](https://www.haskell.org/documentation/)
- [Funkcja `replace` w Haskellu](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-String.html#v:replace)
- [Biblioteka "regex" w Haskellu](https://hackage.haskell.org/package/regex)