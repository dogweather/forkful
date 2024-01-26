---
title:                "Pisanie do standardowego błędu"
html_title:           "Arduino: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
W C piszemy do standardowego błędu (stderr) by rozdzielić normalne dane wyjściowe od komunikatów o błędach. Daje to możliwość łapania i przekierowania ich do logów lub narzędzi do debugowania.

## How to: (Jak to zrobić:)
```C
#include <stdio.h>

int main() {
    fprintf(stderr, "Coś poszło nie tak!\n");
    return 0;
}
```
Wyjście (przykładowe):
```
Coś poszło nie tak!
```

## Deep Dive (Głębsze zanurzenie)
Histerycznie, `stderr` został wprowadzony, by oddzielić komunikaty o błędach od normalnego wyjścia programu (`stdout`). To umożliwia przekierowanie tylko tych dwóch rodzajów danych do różnych miejsc. Alternatywnie, można użyć biblioteki `syslog` na Unixach do logowania systemowego. `stderr` jest buforowanym niezależnie od `stdout`, co oznacza, że komunikaty o błędach pojawiają się od razu, a nie czekają na opróżnienie bufora.

## See Also (Zobacz też)
- GNU C Library documentation on I/O streams: https://www.gnu.org/software/libc/manual/html_node/Streams.html
- Advanced Programming in the UNIX Environment (APUE): Rozdziały o zarządzaniu I/O.
- Stack Overflow - understanding stdout vs stderr: https://stackoverflow.com/questions/3385201/confused-about-stdin-stdout-and-stderr
