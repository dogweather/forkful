---
title:    "C: Lesen von Befehlszeilen-Argumenten"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Befehlszeilenargumenten ist eine grundlegende Fähigkeit in der Programmierung, die es ermöglicht, die Ausführung von Programmen anzupassen und zu steuern. Wenn Sie lernen möchten, wie man Befehlszeilenargumente in C liest, sind Sie hier genau richtig.

## Wie geht man vor?

Zunächst müssen wir das Argumentvektor-Array `argc` und das Argumentliste-Array `argv` aus der `main`-Funktion nutzen, um auf die übergebenen Argumente zuzugreifen. Hier ist ein Beispielcode:

```C
int main(int argc, char *argv[]) {
    // Überprüfe, ob Argumente übergeben wurden
    if (argc > 1) {
        // Drucke die Anzahl der Argumente
        printf("Anzahl der Argumente: %d\n", argc - 1);
        
        // Drucke jedes Argument
        for (int i = 1; i < argc; i++) {
            printf("Argument %d: %s\n", i, argv[i]);
        }
    }
    
    return 0;
}
```

Die `main`-Funktion wird mit zwei Argumenten aufgerufen - die Anzahl der Argumente (`argc`) und die Argumentliste (`argv`). Wir überprüfen zuerst, ob mehr als ein Argument übergeben wurde, da das erste Argument immer der Name des Programms selbst ist. Dann können wir auf die Argumente mit Hilfe von `argv` zugreifen.

Wenn Sie beispielsweise dieses Programm mit dem Befehl `./program argument1 argument2 argument3` aufrufen würden, würde es `Anzahl der Argumente: 3` und `Argument 1: argument1`, `Argument 2: argument2` und `Argument 3: argument3` ausgeben.

## Tiefer tauchen

Es ist wichtig zu beachten, dass `argv` ein Array von Zeigern auf Zeichenketten ist. Das bedeutet, dass jedes Argument eine Zeichenkette ist und `argv` ein Array von Zeigern auf diese Zeichenketten ist. Sie können auf die einzelnen Zeichenketten wie folgt zugreifen: `argv[0]` ist der Name des Programms, `argv[1]` ist das erste Argument, `argv[2]` ist das zweite Argument und so weiter.

Sie können auch verschiedene Funktionen wie `strcmp` verwenden, um Argumente zu vergleichen, oder `atoi` für die Konvertierung von Zeichenketten in Zahlen.

Um mehr darüber zu erfahren, wie Befehlszeilenargumente in C funktionieren, können Sie die Dokumentation oder andere Quellen im Internet konsultieren.

## Siehe auch

- [Dokumentation über Command-Line-Argumente in C](https://en.cppreference.com/w/c/language/main_function)
- [Tutorial über Command-Line-Argumente in C](https://www.programiz.com/c-programming/c-command-line-arguments)
- [Stack Overflow - Command-Line-Argumente in C lesen](https://stackoverflow.com/questions/3024197/reading-command-line-arguments-in-c)