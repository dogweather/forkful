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

## Co i dlaczego?

Obliczanie daty w przyszłości lub przeszłości to proces, w którym programista używa bieżącej daty jako podstawy do obliczenia nowej daty, która jest położona kilka dni, miesięcy lub lat w przyszłości lub przeszłości. 

Programiści często potrzebują takiej funkcjonalności, aby zbadać różne scenariusze lub przewidywać przyszłe wydarzenia w swoim kodzie.

## Jak to zrobić:

```Haskell
addDays :: Int -> Day -> Day
subDays :: Int -> Day -> Day

addDays 10 (fromGregorian 2021 11 20) -- 30 listopada 2021
subDays 5 (fromGregorian 2021 12 1)  -- 26 listopada 2021
```

W powyższym przykładzie używamy funkcji ```addDays``` i ```subDays``` z biblioteki standardowej ```Data.Time.Calendar``` w celu dodania lub odjęcia określonej liczby dni od danej daty. Możemy również użyć innych funkcji, takich jak ```addMonths``` lub ```addYears```, aby dodać lub odjąć miesiące lub lata od daty.

## Głębsze zagłębienie:

Obliczanie daty w przyszłości lub przeszłości jest niezbędnym elementem w wielu aplikacjach, takich jak kalendarze, systemy rezerwacji i prognozy pogody. Dlatego programiści często używają gotowych rozwiązań, takich jak biblioteka ```Data.Time.Calendar```, która dostarcza funkcji do obliczania dat.

Alternatywnie, możemy również użyć funkcji ```addUTCTime``` z biblioteki ```Data.Time.Clock```, aby dodać lub odjąć czas od daty, co może być wygodniejsze w niektórych przypadkach.

W przypadku obliczania daty w przeszłości musimy również wziąć pod uwagę czas letni i czas standardowy, co wymaga zaawansowanej implementacji w celu uniknięcia błędów.

## Zobacz także:

- Dokumentacja biblioteki ```Data.Time.Calendar``` dla więcej funkcji i przykładów: https://hackage.haskell.org/package/time-1.9/docs/Data-Time-Calendar.html
- Poradnik dotyczący dat i czasu w Haskellu: https://markkarpov.com/tutorial/datetime.html