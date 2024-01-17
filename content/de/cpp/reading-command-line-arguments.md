---
title:                "Lesen von Befehlszeilenargumenten"
html_title:           "C++: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?

Lesen von Kommandozeilenargumenten ist eine grundlegende Fähigkeit für Programmierer. Es ermöglicht Programmen, Befehlszeilenargumente zu erhalten und auf sie zu reagieren. Dies kann nützlich sein, um verschiedene Einstellungen, Dateinamen oder andere Parameter für das Programm festzulegen.

## Wie geht das?

Die meisten Programmiersprachen, einschließlich C++, bieten eingebaute Methoden oder Bibliotheken, um Kommandozeilenargumente zu lesen. In C++ können wir die Funktion `main` verwenden, die zwei Parameter erwartet: `argc` und `argv`. `argc` gibt die Anzahl der Argumente an, während `argv` ein Array von C-Strings ist, das die tatsächlichen Argumente enthält.

Um alle Argumente zu lesen, können wir eine Schleife verwenden und `argv` durchlaufen. Zum Beispiel:

```
int main(int argc, char* argv[]) {
    for (int i = 0; i < argc; i++) {
        cout << argv[i] << endl;
    }
    return 0;
}
```

Wenn wir dieses Programm mit dem Befehl `./program argument1 argument2` aufrufen, wird es die Argumente `argument1` und `argument2` auf der Konsole ausgeben.

## Tiefere Einblicke

Das Lesen von Kommandozeilenargumenten ist seit langem eine bewährte Methode in der Programmierung. Vor der Verwendung von grafischen Benutzeroberflächen war dies die einzige Möglichkeit, wie Benutzer mit Computern interagieren konnten. Alternativen zu Kommandozeilenargumenten sind beispielsweise Konfigurationsdateien oder Benutzereingaben während der Programmausführung.

Wenn wir tiefer gehen, können wir uns ansehen, wie Kommandozeilenargumente in C++ implementiert sind. Beispielsweise kaufen sich `argc` und `argv` direkt auf die C-Standardbibliothek `stdlib.h` und ihre Funktion `getopt`. Diese Funktion ermöglicht es uns, Optionen und Argumente in einer für Unix-ähnliche Systeme üblichen Weise zu lesen.

## Siehe auch

Um mehr über das Lesen von Kommandozeilenargumenten in C++ zu erfahren, können Sie die offizielle Dokumentation von C++ oder andere Online-Tutorials lesen. Hier sind einige weitere hilfreiche Links:

- https://www.tutorialspoint.com/cplusplus/cpp_command_line_arguments.htm
- https://www.learncpp.com/cpp-tutorial/command-line-arguments/
- https://en.cppreference.com/w/cpp/language/main_function