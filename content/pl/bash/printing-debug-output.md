---
title:                "Drukowanie komunikatów debugowania"
html_title:           "Haskell: Drukowanie komunikatów debugowania"
simple_title:         "Drukowanie komunikatów debugowania"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

# Jak używać wydruku debugowania w Bashu

## Co i dlaczego?

Wydruk debugowania to nasz najlepszy przyjaciel przy szukaniu błędów. Pokazuje nam wartości zmiennych, pozwala sprawdzać logikę kroku po kroku.

## Jak to zrobić:

W Bashu wydruk debugowania robimy tak:

```Bash
#!/bin/bash
DEBUG=true
if $DEBUG 
then
    echo "Debugowanie jest włączone"
fi
```
Output:
```
Debugowanie jest włączone
```

Możemy też wykorzystać funkcję do debugowania:

```Bash
#!/bin/bash
DEBUG=true
debug() {
   if $DEBUG 
   then
      echo "Debug: $*"
   fi
}

debug "Zmienna x ma wartość $x"
```
Output:
```
Debug: Zmienna x ma wartość 5
```

## Głębsze spojrzenie

Historia: Bash nie miał wbudowanego debugowania. Programiści znaleźli jednak triki, jak to robić.

Alternatives: Możemy użyć wbudowanych skryptów debugujących, jak `set -x` albo `set -v`.

Szczegóły implementacji: W Bashu używamy wydruku debugowania na różne sposoby - przez `set`, `echo`, `printf` czy zmienną środowiskową DEBUG.

## Zobacz również

- Debugowanie Bashu: https://www.gnu.org/software/bash/manual/html_node/The-Set-Builtin.html
- Skrypty debugujące: https://www.linuxjournal.com/content/debugging-bash-scripts
- Przekierowanie wyjścia błędów: https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Redirections