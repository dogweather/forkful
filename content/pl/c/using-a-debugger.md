---
title:                "Korzystanie z debugera"
date:                  2024-01-26T03:48:25.668152-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z debugera"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/using-a-debugger.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Debugger to narzędzie, które pozwala na inspekcję kodu C podczas jego wykonywania, krok po kroku, w celu odnalezienia błędów. Programiści używają debuggerów do zrozumienia zachowania swojego kodu, naprawy problemów i optymalizacji wydajności bez grania w zgadywanki.

## Jak to zrobić:
Załóżmy, że pracujesz nad prostym programem w C, który oblicza silnię liczby, ale pojawił się problem. Aby użyć debuggera takiego jak `gdb` (GNU Debugger), najpierw skompiluj z flagą `-g`, aby dołączyć informacje dla debuggera:

```c
// kompilacja z: gcc factorial.c -o factorial -g
#include <stdio.h>

long factorial(int n) {
    if (n < 0) return 0; // Prosta kontrola ujemnego wejścia
    long result = 1;
    while (n > 1)
        result *= n--;
    return result;
}

int main() {
    int number = 5;
    long result = factorial(number);
    printf("Silnia liczby %d wynosi %ld\n", number, result);
    return 0;
}
```

Następnie uruchom go w gdb:

```shell
$ gdb ./factorial
```

Ustaw punkt przerwania na funkcji `factorial` i uruchom program:

```gdb
(gdb) break factorial
(gdb) run
```

Kiedy program zatrzyma się na punkcie przerwania, przejdź przez każdą linię używając `next` lub `n` i inspekcjonuj zmienne przy pomocy `print` lub `p`:

```gdb
(gdb) next
(gdb) print result
$1 = 1
```

Przykładowe wyjście dostarczy wartości w czasie rzeczywistym i przebieg wykonania programu.

## Pogłębiona analiza
Debuggery istnieją od lat 60., ewoluując od prostych monitorów do złożonych aplikacji z interfejsem graficznym. Stare, oparte na wydrukach debuggowanie było powszechne przed rozwojem dojrzałych debuggerów. Alternatywy dla `gdb` obejmują `lldb`, `dbx`, czy debuggery zintegrowane z IDE, takie jak te w Visual Studio czy CLion.

Przy pracy z debuggerami implementacja się różni - niektóre mogą łapać błędy czasu wykonania, eksaminować pamięć, a nawet odwracać wykonanie programu. `gdb` może dołączać do działających procesów, co pozwala na debuggowanie już działającego oprogramowania, co jest zaletą przy naprawianiu błędów w systemach na żywo.

## Zobacz także
- Debugger GNU (GDB): https://www.gnu.org/software/gdb/documentation/
- Debuggowanie z GDB: https://sourceware.org/gdb/current/onlinedocs/gdb
- Debugger LLDB: https://lldb.llvm.org/use/tutorial.html
- Techniki debuggowania w C: http://www.cprogramming.com/debugging/debugging.html
