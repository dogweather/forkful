---
title:                "Einsatz eines Debuggers"
date:                  2024-01-26T03:47:36.064474-07:00
model:                 gpt-4-0125-preview
simple_title:         "Einsatz eines Debuggers"

category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/using-a-debugger.md"
---

{{< edit_this_page >}}

## Was & Warum?
Ein Debugger ist ein Werkzeug, das es Ihnen ermöglicht, Ihren C-Code während des Ausführens Schritt für Schritt zu inspizieren, um Fehler aufzuspüren. Programmierer nutzen Debugger, um zu verstehen, wie ihr Code sich verhält, Probleme zu beheben und die Leistung zu optimieren, ohne raten zu müssen.

## Wie:
Nehmen wir an, Sie arbeiten mit einem einfachen C-Programm, das die Fakultät einer Zahl berechnet, aber es gibt einen Fehler. Um einen Debugger wie `gdb` (GNU Debugger) zu verwenden, kompilieren Sie zuerst mit dem Flag `-g`, um Debug-Informationen einzuschließen:

```c
// kompilieren mit: gcc factorial.c -o factorial -g
#include <stdio.h>

long factorial(int n) {
    if (n < 0) return 0; // Eine einfache Überprüfung auf negative Eingabe
    long result = 1;
    while (n > 1)
        result *= n--;
    return result;
}

int main() {
    int number = 5;
    long result = factorial(number);
    printf("Die Fakultät von %d ist %ld\n", number, result);
    return 0;
}
```

Dann führen Sie es in gdb aus:

```shell
$ gdb ./factorial
```

Setzen Sie einen Haltepunkt bei der Funktion `factorial` und starten Sie das Programm:

```gdb
(gdb) break factorial
(gdb) run
```

Wenn es den Haltepunkt erreicht, durchlaufen Sie jede Zeile mit `next` oder `n` und inspizieren Variablen mit `print` oder `p`:

```gdb
(gdb) next
(gdb) print result
$1 = 1
```

Beispielausgaben liefern Echtzeitwerte und den Programmablauf.

## Tiefergehende Betrachtung
Debugger gibt es schon seit den 1960er Jahren. Sie haben sich von einfachen Monitoren zu komplexen, GUI-basierten Anwendungen entwickelt. Die altmodische, auf Ausdrucken basierende Fehlersuche war verbreitet, bevor ausgereifte Debugger entwickelt wurden. Alternativen zu `gdb` umfassen `lldb`, `dbx` oder IDE-integrierte Debugger wie jene in Visual Studio oder CLion.

Beim Umgang mit Debuggern variiert die Implementierung – manche können Laufzeitfehler erfassen, den Speicher untersuchen oder sogar die Ausführung eines Programms umkehren. `gdb` kann sich an laufende Prozesse anhängen und erlaubt damit das Debuggen bereits laufender Software, ein Segen für die Behebung von Live-Systemfehlern.

## Siehe auch
- GNU Debugger (GDB): https://www.gnu.org/software/gdb/documentation/
- Debugging mit GDB: https://sourceware.org/gdb/current/onlinedocs/gdb
- LLDB Debugger: https://lldb.llvm.org/use/tutorial.html
- Debugging-Techniken in C: http://www.cprogramming.com/debugging/debugging.html
