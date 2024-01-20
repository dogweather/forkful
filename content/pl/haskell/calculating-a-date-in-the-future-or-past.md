---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Haskell: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Obliczanie daty w przyszłości/późniejszej to proces wyznaczania konkretnego dnia, miesiąca i roku, który występuje po lub przed zadaną datą. Programiści robią to, aby przewidzieć lub zrekonstruować daty i wydarzenia w aplikacjach takich jak kalendarze, planery czy systemy zarządzania projektami.

## Jak to zrobić:

Użyjąc modułu Data.Time dostarczanego przez bibliotekę time w Haskellu (aktualna wersja), możemy łatwo obliczyć datę w przyszłości lub przeszłości. Oto przykład:

```haskell
import Data.Time

main :: IO ()
main = do 
    czas <- getCurrentTime
    let dzien = utctDay czas
    print (addDays 5 dzien) -- Dodaje 5 dni do bieżącej daty
    print (addDays (-7) dzien) -- Odejmuje 7 dni od bieżącej daty
```
Wejście programu jest aktualnym datą i godziną, a wyjście to przyszła lub przeszła data.

## Deep Dive

Obliczanie daty w przyszłości lub przeszłości ma wiele zastosowań, nie tylko w obliczeniach, ale także w historii i prognozowaniu. Na przykład w systemach bankowych, potrzeba prognozowania dat przyszłych płatności.

Alternatywą dla wbudowanej funkcji `addDays` w Haskellu jest ręczne dodawanie lub odejmowanie dni od daty. To jednak może prowadzić do błędów, jak na przykład nieuwzględnienie roku przestępnego.

## Zobacz też

"Haskell/Date and Time" - podręcznik dostępny na Wikibooks: [https://en.wikibooks.org/wiki/Haskell/Date_and_Time](https://en.wikibooks.org/wiki/Haskell/Date_and_Time)
"Dodatek do czasu" - dokumentacja funkcji `addDays` w Haskellu: [https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Calendar.html#v:addDays](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Calendar.html#v:addDays)