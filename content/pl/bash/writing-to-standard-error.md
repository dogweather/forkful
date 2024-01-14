---
title:                "Bash: Pisanie do standardowego wyjścia błędów"
simple_title:         "Pisanie do standardowego wyjścia błędów"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie do standardowego błędu jest nieodłączną częścią programowania w Bash. Jest to prosty, ale bardzo użyteczny sposób na wyświetlanie błędów i komunikatów diagnostycznych podczas wykonywania skryptów lub poleceń w terminalu.

## Jak to zrobić

Możesz pisać do standardowego błędu przy użyciu polecenia `echo` wraz z argumentem `>&2` lub wykorzystując operator `2>`, który przekierowuje wyjście do standardowego błędu. Przykładowo:

```
Bash

echo "Nie udało się odnaleźć pliku!" >&2

ls nieistniejacy_plik 2> bledy.txt
```

W pierwszym przykładzie wyświetlamy komunikat o błędzie, a w drugim zapisujemy błędy do pliku `bledy.txt`. Możesz również użyć `2>&1` aby przekierować standardowy błąd do standardowego wyjścia.

## Wszczepienie się głębiej

Warto pamiętać, że domyślnie standardowy błąd jest przekierowywany do konsoli, dlatego warto używać przekierowania wyjścia do pliku lub potoku, aby móc później przejrzeć komunikaty błędów.

Dodatkowo, warto również zwrócić uwagę na wykorzystanie zmiennych `$?`, które przechowuje kod zakończenia ostatnio wykonywanego polecenia. Możesz wykorzystać go w warunkach, aby obsłużyć odpowiednio wyjątki i błędy.

## Zobacz również

- [BashGuide - Standardowe wejście i wyjście](http://mywiki.wooledge.org/BashGuide/InputAndOutput)
- [BashRef - Wymiana danych z procesami](https://ss64.com/bash/syntax-redirection.html)
- [BashTutorial - Przekierowywanie wyjścia i wejścia](https://linuxconfig.org/bash-scripting-tutorial-for-beginners#h1-5-1-standard-output-and-error)