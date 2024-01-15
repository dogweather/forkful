---
title:                "Schreiben auf die Standardfehlerausgabe"
html_title:           "C: Schreiben auf die Standardfehlerausgabe"
simple_title:         "Schreiben auf die Standardfehlerausgabe"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Warum

Neben der Standardausgabe (STDOUT) gibt es in der C-Programmierung auch die Möglichkeit, Daten an den Standardfehlerausgang (STDERR) zu schreiben. Dies ist zum Beispiel nützlich, um Fehlermeldungen oder Debugging-Informationen während der Ausführung eines Programms auszugeben.

# Wie geht das?

Die Funktion `fprintf()` kann verwendet werden, um Daten an den Standardfehlerausgang zu schreiben. Sie benötigt als ersten Parameter einen Zeiger auf den Ausgabestrom `stderr` und als zweiten Parameter einen Format-String, der angibt, wie die Daten formatiert werden sollen. Hier ein Beispielcode:

```C
#include <stdio.h>

int main(){
    int num = 5;
    fprintf(stderr, "Die Zahl ist %d\n", num);
    return 0;
}
```

Die Ausgabe dieses Programms wird auf dem Standardfehlerausgang geschrieben und somit als Fehlermeldung angezeigt:

```
Die Zahl ist 5
```

# Tiefer eintauchen

Die Verwendung von `fprintf()` zum Schreiben auf den Standardfehlerausgang ist besonders nützlich, wenn es um das Debugging von Programmen geht. Durch die Ausgabe von Variablenwerten oder Meldungen auf dem Standardfehlerausgang können Entwickler schnell sehen, wo mögliche Fehler auftreten.

Es ist auch wichtig zu beachten, dass der Standardfehlerausgang nicht gepuffert ist, im Gegensatz zur Standardausgabe. Das bedeutet, dass die Ausgabe sofort angezeigt wird und nicht auf eine neue Zeile oder auf ein `fflush()` wartet.

# Siehe auch

- [fprintf() Dokumentation](https://www.cplusplus.com/reference/cstdio/fprintf/)
- [Quick Guide to C Error Handling](https://www.codingame.com/playgrounds/14213/how-to-handle-errors-in-c/error-handling)