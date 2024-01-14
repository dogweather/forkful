---
title:    "C: An das Standardfehler-Schreiben"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben auf die Standardfehlerausgabe ist eine wichtige Fähigkeit für jeden C-Programmierer. Es ermöglicht es uns, Fehler und andere wichtige Ausgaben während der Laufzeit eines Programms zu erfassen. Ohne diese Fähigkeit wäre es sehr schwierig, Probleme in unserem Code zu identifizieren und zu beheben.

## Wie man es macht

Um auf die Standardfehlerausgabe zu schreiben, benötigen wir die Funktion `fprintf()`, die in der Standardbibliothek von C enthalten ist. Diese Funktion nimmt drei Argumente an: den Dateizeiger für die Ausgabe (hier "stderr" für die Standardfehlerausgabe), einen Formatstring und optional zusätzliche Argumente, die in den Formatstring eingefügt werden.
 
```C
#include <stdio.h>

int main(){
    fprintf(stderr, "Dies ist ein Beispiel für eine Ausgabe auf die Standardfehlerausgabe\n");
    
    // Kann auch mit Variablen verwendet werden
    int zahl = 42;
    fprintf(stderr, "Die Antwort auf alles ist %d\n", zahl);
    
    return 0;
}
```

Dieses Beispiel zeigt, wie wir die `fprintf()`-Funktion verwenden können, um Text und Variablenwerte auf die Standardfehlerausgabe zu schreiben. Eine wichtige Sache zu beachten ist, dass die Ausgabe auf die Standardfehlerausgabe nicht gepuffert wird, was bedeutet, dass sie sofort auf dem Bildschirm angezeigt wird.

## Tiefentauchen

Wenn wir tiefer in die Funktion `fprintf()` eintauchen, werden wir feststellen, dass sie viel mächtiger ist als nur eine Ausgabe auf die Standardfehlerausgabe. Wir können verschiedene Modifier im Formatstring verwenden, um die Ausgabe zu steuern. Zum Beispiel können wir mit `%s` eine Zeichenkette, mit `%d` eine ganze Zahl und mit `%f` eine Gleitkommazahl in die Ausgabe einfügen.

Es ist auch möglich, `fprintf()` zu verwenden, um auf andere Dateien zu schreiben, nicht nur auf die Standardfehlerausgabe. Indem wir den Dateizeiger für die Ausgabe ändern, können wir auf verschiedene Dateien schreiben. Dies kann nützlich sein, wenn wir Protokolldateien erstellen wollen, um die Ausführung unseres Programms zu verfolgen.

## Siehe auch

- [Die offizielle Dokumentation zu `fprintf()`](https://www.mkssoftware.com/docs/man3/fprintf.3.asp)
- [Ein Tutorial zur Benutzung von Standard- und Fehlerausgabe in C](https://www.codingunit.com/c-tutorial-the-functions-printf-and-scanf)
- [Weitere Informationen zu C-Programmierung und Fehlerbehandlung](https://www.geeksforgeeks.org/error-handling-c-programs/)