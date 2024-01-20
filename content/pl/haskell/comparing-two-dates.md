---
title:                "Porównywanie dwóch dat"
html_title:           "C++: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

---

## Co & Dlaczego?
Porównanie dwóch dat to zasadniczo sprawdzenie, która data jest wcześniejsza, a która późniejsza. Programiści często wykonują to porównanie, aby sortować wydarzenia chronologicznie, analizować trendy czasowe czy kontrolować ważność certyfikatów.

## Jak to zrobić:
Aby porównać dwie daty w Haskellu, używamy funkcji `diffUTCTime`, która zwraca różnicę między dwiema datami w sekundach. Poniżej znajduje się przykład porównania dwóch dat:

```Haskell
import Data.Time.Clock

dateComparison :: UTCTime -> UTCTime -> NominalDiffTime
dateComparison date1 date2 = diffUTCTime date1 date2
```

Ważne jest, aby pamiętać, że `diffUTCTime` zwraca wartość dodatnią, jeśli pierwsza data jest późniejsza, a wartość ujemną, jeśli druga data jest późniejsza.

## Na głębszą wodę:
Porównywanie dat ma swoje korzenie w zarządzaniu bazami danych i bywa często używane w analizie danych. W Haskellu, alternatywą dla `diffUTCTime` jest `diffDays` z `Data.Time.Calendar`, która zwraca różnicę w dniach (ale jest mniej precyzyjna). W implementednym kodzie najważniejszą rzeczą jest poprawne użycie typów czasu - `UTCTime` i `NominalDiffTime` - oraz funkcji z `Data.Time.Clock`.

## Zobacz też:
- [Dokumentacja biblioteki Data.Time](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [Poradnik Haskell](https://haskellbook.com/) dla dogłębnej nauki języka.

---