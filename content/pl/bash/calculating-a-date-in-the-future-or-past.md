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

# Obliczanie daty w przeszłości lub przyszłości w Bashu 

## Co to i dlaczego?

Obliczanie daty w przeszłości lub przyszłości polega na determinowaniu daty, która jest pewien okres czasu od teraz. Programiści robią to np. do monitoringu, przypomnień lub planowania.

## Jak to zrobić:

Zakodujmy to szybko. W Bashu, możemy obliczyć datę w przyszłości lub przeszłości używając funkcji `date`.

```Bash 
# Obliczanie daty za 5 dni
date -d '+5 day'

# Obliczanie daty sprzed 5 dni
date -d '-5 day'
```

Output:

```Bash
# Za 5 dni
2022-03-05

# Sprzed 5 dni
2022-02-23
```

## Deep Dive:

- **Kontekst historyczny:** Bash pozwala obliczać daty w przyszłości lub przeszłości od wersji 2.1 wydanej w 1999 r.
- **Alternatywy:** Można także używać Pythona lub JavaScriptu do obliczania dat, ale skomplikuje to skrypt i wymagać będzie dodatkowych narzędzi.
- **Szczegóły implementacji:** W Bashu, `date` jest wbudowaną komendą, która wykorzystuje bibliotekę C `time.h` do przeliczania sekund na strukturę czasu.

## Zobacz także:

- Szczegółowy opis składni i używania `date` w Bashu: [https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- Porównanie różnych języków programowania do obliczania daty: [https://www.codeproject.com/Articles/2750/Date-and-Time-in-C-vs-NET](https://www.codeproject.com/Articles/2750/Date-and-Time-in-C-vs-NET)
- Obliczanie daty w przeszłości/przyszłości w Pythonie: [https://docs.python.org/3/library/datetime.html#timedelta-objects](https://docs.python.org/3/library/datetime.html#timedelta-objects)
- Obliczanie daty w przeszłości/przyszłości w JavaScript: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date#calculate_elapsed_time](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date#calculate_elapsed_time)