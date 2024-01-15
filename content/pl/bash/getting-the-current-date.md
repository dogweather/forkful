---
title:                "Pobieranie bieżącej daty"
html_title:           "Bash: Pobieranie bieżącej daty"
simple_title:         "Pobieranie bieżącej daty"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Pobranie aktualnej daty jest częstym zadaniem w programowaniu, często wykorzystywanym w skryptach i automatycznych procesach. Może być przydatne do tworzenia nazw plików, logowania zdarzeń lub zabezpieczania danych.

## Jak to zrobić

Możesz łatwo pobrać aktualną datę w Bash za pomocą wbudowanej funkcji `date`. Wystarczy wpisać poniższą komendę w terminalu:

```Bash
date
```

Wyświetli ona aktualną datę i godzinę w formacie ustawionym na Twoim komputerze. Jeśli chcesz inny format, możesz użyć opcji `-d` i podać żądany format. Na przykład:

```Bash
date -d "5 days ago"
```

Spowoduje wyświetlenie daty, która była 5 dni temu. Możesz także wykorzystać funkcję `printf` w połączeniu z `date`, aby manipulować wyświetlanym formatem daty. Na przykład, aby wyświetlić datę w formacie DD/MM/RRRR, użyj następującej komendy:

```Bash
printf "%(%d/%m/%Y)T\n" $(date)
```

## Głębszy zanurzenie

Funkcja `date` może również być wykorzystywana do wyświetlania daty w różnych strefach czasowych, porównywania dat lub przekształcania ich do innego formatu. Możesz znaleźć więcej informacji w dokumentacji systemu Unix dotyczącej funkcji `date`.

Możesz także wykorzystać zmienne środowiskowe, takie jak `TZ`, aby ustawić strefę czasową dla danej komendy czy skryptu. Na przykład:

```Bash
TZ=America/Los_Angeles date +"%T %Z"
```

Wyświetli aktualną godzinę w strefie czasowej Los Angeles.

## Zobacz również

- Dokumentacja funkcji `date` w systemie Unix: http://man7.org/linux/man-pages/man1/date.1.html
- Inne funkcje systemu Unix, które mogą być przydatne przy pracy z datami: http://man7.org/linux/man-pages/man3/strftime.3.html
- Przydatna lista stref czasowych: https://en.wikipedia.org/wiki/List_of_tz_database_time_zones