---
date: 2024-01-26 04:12:02.092219-07:00
description: "Wie: C++ verf\xFCgt nicht \xFCber eine integrierte REPL, aber Werkzeuge\
  \ wie Cling bieten diese M\xF6glichkeit. So verwenden Sie Cling, um die Summe zweier\
  \ Zahlen\u2026"
lastmod: '2024-03-13T22:44:54.186225-06:00'
model: gpt-4-0125-preview
summary: "C++ verf\xFCgt nicht \xFCber eine integrierte REPL, aber Werkzeuge wie Cling\
  \ bieten diese M\xF6glichkeit."
title: Nutzung einer interaktiven Shell (REPL)
weight: 34
---

## Wie:
C++ verfügt nicht über eine integrierte REPL, aber Werkzeuge wie Cling bieten diese Möglichkeit. So verwenden Sie Cling, um die Summe zweier Zahlen zu berechnen:

```C++
#include <iostream>

int main() {
    int a = 5;
    int b = 7;
    std::cout << "Die Summe ist: " << a + b << std::endl;
    return 0;
}

// Ausgabe:
// Die Summe ist: 12
```

Starten Sie Cling und geben Sie den Code Zeile für Zeile ein, wobei Sie die Ausgabe nach jedem Befehl beobachten. Es ist ein sofortiges Feedback, ohne Kompilierung.

## Tiefergehend
REPLs sind üblich für Sprachen wie Python oder Lisp und existieren seit den 1960er Jahren. Für C++, eine kompilierte Sprache, passt das Konzept nicht so natürlich, weshalb Werkzeuge wie Cling existieren - sie interpretieren C++ on the fly. Alternativen umfassen Online-Compiler oder kleinskalige Testprogramme, die traditionell kompiliert werden. Cling basiert auf LLVM und Clang und bietet eine Brücke, damit C++ auf interpretierte Weise verwendet werden kann.

## Siehe auch
- [Cling](https://root.cern/cling/): Ein interaktiver C++-Interpreter, aufgebaut auf den LLVM- und Clang-Bibliotheken.
- [Jupyter Notebooks](https://jupyter.org/): Bietet eine interaktive Shell innerhalb einer Notizbuchumgebung, unterstützt C++ durch den xeus-cling-Kernel.
- [LLVM](https://llvm.org/): Eine Sammlung von modularen und wiederverwendbaren Compiler- und Werkzeugkettentechnologien, auf denen Cling aufbaut.
