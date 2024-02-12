---
title:                "Lesen von Kommandozeilenargumenten"
aliases:
- /de/cpp/reading-command-line-arguments.md
date:                  2024-01-20T17:55:27.603434-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lesen von Kommandozeilenargumenten"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?
Kommandozeilenargumente ermöglichen es einem Programm, beim Start Eingaben zu empfangen. Das ist praktisch, um dem Programm zu sagen, was es tun soll, ohne dass der Benutzer das Programm selbst ändern muss.

## So geht's:
In C++ kannst du mit `int argc` und `char *argv[]` arbeiten, die im Hauptfunktionsheader stehen. `argc` zählt die Argumente, `argv` ist ein Array der Argumentwerte.

```C++
#include <iostream>

int main(int argc, char *argv[]) {
    std::cout << "Anzahl Argumente: " << argc << std::endl;
    for (int i = 0; i < argc; i++) {
        std::cout << "Argument " << i << ": " << argv[i] << std::endl;
    }
    return 0;
}
```

Ausgabe könnte sein für `./programm Hallo Welt`:
```
Anzahl Argumente: 3
Argument 0: ./programm
Argument 1: Hallo
Argument 2: Welt
```

## Tiefgang:
Kommandozeilenargumente sind ein Langzeitstandard. Sie gehen zurück auf C und UNIX. In modernen C++ Projekten nutzen einige Entwickler Bibliotheken wie `boost::program_options` oder `TCLAP` für mehr Flexibilität und leichtere Syntax.

Die Standardimplementierung (`argc` und `argv`) hält Dinge einfach, kann aber knifflig werden, wenn Argumente zusätzliche Verarbeitung erfordern oder nicht nur Strings sind.

Eine Alternative für einfache Fälle könnte die Environment Variable sein, aber das ist weniger direkt und nutzerfreundlich.

## Siehe auch:
- CppReference über `main()`: https://en.cppreference.com/w/cpp/language/main_function
- Boost Program Options Bibliothek: https://www.boost.org/doc/libs/release/libs/program_options/
- TCLAP, eine templatelastige Bibliothek: http://tclap.sourceforge.net/
- POSIX-Standard für Argumente: https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap12.html
