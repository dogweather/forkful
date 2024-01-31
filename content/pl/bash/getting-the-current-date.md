---
title:                "Pobieranie aktualnej daty"
date:                  2024-01-20T15:12:53.293410-07:00
html_title:           "Bash: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
W Bashu polecenie daty zwraca aktualną datę i czas, a programiści wykorzystują to do logowania, harmonogramowania zadań lub do wyświetlania informacji o czasie w skryptach.

## How to:
Aby wyświetlić aktualną datę i czas, użyj polecenia `date`. Sprawdź kilka przykładów:

```Bash
# Wyświetla aktualną datę i czas w domyślnym formacie
date

# Wyświetla datę w określonym formacie, np. RRRR-MM-DD
date +"%Y-%m-%d"

# Wyświetla czas w określonym formacie, np. GG:MM:SS
date +"%H:%M:%S"
```

Przykładowe wyjścia:

```Bash
Tue Mar  9 15:20:45 PST 2021
2021-03-09
15:20:45
```

## Deep Dive
Polecenie `date` w systemach Unix-like, w tym Linux, pochodzi z tradycji narzędzi systemowych zapewnionych przez AT&T System V oraz BSD (Berkeley Software Distribution). Alternatywnie, możesz użyć `strftime` w skryptach Bash do jeszcze większej kontroli nad formatowaniem, a `date` jest często używane razem z cronem do planowania zadań. 

Implementacja `date` opiera się na standardzie C POSIX library i może pokazywać różne strefy czasowe, co jest przydatne w pracy globalnej lub z rozproszonymi systemami. Dodatkowo, dla zaawansowanego formatowania możesz łączyć różne sekwencje formatowania w jednym wywołaniu polecenia.

## See Also
- GNU Coreutils `date`: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Bash scripting cheatsheet: https://devhints.io/bash
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/
