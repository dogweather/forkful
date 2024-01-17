---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Bash: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Obliczanie daty w przyszłości lub w przeszłości jest procesem, w którym programista wykorzystuje język Bash do obliczenia daty na podstawie podanej daty początkowej i ilości dni. Jest to przydatne narzędzie w różnych sytuacjach, takich jak generowanie raportów, kontrola gotowości projektu lub planowanie zadań.

## Jak to zrobić:
Możesz użyć polecenia ```date``` w Bashu, aby obliczyć datę w przyszłości lub w przeszłości. Aby obliczyć datę w przyszłości, użyj ```date -d "YYYY-MM-DD + 5 days" ``` gdzie "5 days" to ilość dni, którą chcesz dodać do daty początkowej. Aby obliczyć datę w przeszłości, użyj ```date -d "YYYY-MM-DD - 5 days"``` gdzie "5 days" to ilość dni, którą chcesz odjąć od daty początkowej. Poniżej znajdują się przykładowe wyjścia dla tych poleceń:

```
Bash: date -d "2021-01-01 + 5 days"
Output: Tue Jan 5 00:00:00 PST 2021

Bash: date -d "2021-01-01 - 5 days"
Output: Sun Dec 27 00:00:00 PST 2020
```

## Głębszy zanurzenie:
Obliczanie daty w przyszłości lub w przeszłości jest możliwe dzięki użyciu programu GNU `date`, który dostarcza wiele opcji, takich jak formatowanie daty czy obliczanie czasu pomiędzy dwoma datami. Alternatywnym narzędziem do obliczania daty w Bashu jest program `cal`, który służy do wyświetlania kalendarza.

## Zobacz także:
- [Dokumentacja programu `date` w Bashu] (https://ss64.com/bash/date.html)
- [Podręcznik programisty Bash] (https://www.tldp.org/LDP/abs/html/)
- [Dokumentacja programu `cal` w Bashu] (https://ss64.com/bash/cal.html)