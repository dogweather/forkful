---
title:                "Haskell: Wyodrębnianie podciągów"
simple_title:         "Wyodrębnianie podciągów"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Extrakcja podciągów jest ważną umiejętnością w programowaniu, która pozwala nam na wydobycie tylko tych części tekstu, które nas interesują. Jest to niezbędne w różnych sytuacjach, na przykład przy przetwarzaniu i analizie danych tekstowych.

## Jak to zrobić

```Haskell
-- Definiowanie przykładowego tekstu
tekst = "Cześć! Nazywam się Kasia i jestem programistką."

-- Wydobycie pierwszych 5 liter
take 5 tekst
-- Output: "Cześć"

-- Wydobycie ostatnich 10 liter
drop (length tekst - 10) tekst
-- Output: "jestem programistką."
```

Okazuje się, że istnieje wiele przydatnych funkcji w Haskellu, które pozwalają nam na ekstrakcję podciągów w różnych sposób. Na przykład funkcja `take` pozwala nam na wydobycie określonej liczby elementów na początku tekstu, podczas gdy `drop` pozwala na wydobycie na końcu tekstu. Aby uzyskać dłuższy podciąg, możemy także połączyć te dwie funkcje w jednym wywołaniu.

## Głębsza analiza

Za pomocą funkcji `take` i `drop` możemy wydobyć tylko określony fragment tekstu, ale co jeśli chcemy wyodrębnić część tekstu na podstawie warunków? W tym przypadku możemy skorzystać z funkcji `takeWhile` i `dropWhile`, które pozwalają nam na wydobycie podciągu aż do momentu, gdy warunek zostanie spełniony.

```Haskell
-- Definiowanie przykładowego tekstu
tekst = "Dzisiaj jest ładna, słoneczna pogoda."

-- Wydobycie wszystkich słów po słowie "jest"
dropWhile (/= "jest") (words tekst)
-- Output: ["jest", "ładna", "słoneczna", "pogoda."]
```

W ten sposób możemy bardzo precyzyjnie wyodrębnić tylko potrzebne nam fragmenty tekstu.

## Zobacz także

- [Dokumentacja Haskell](https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#g:20)
- [Tutorial: Podstawy Haskell](https://learnhaskell.com/)