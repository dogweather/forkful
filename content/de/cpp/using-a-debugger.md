---
title:                "Einsatz eines Debuggers"
date:                  2024-01-26T03:47:41.210291-07:00
model:                 gpt-4-0125-preview
simple_title:         "Einsatz eines Debuggers"

category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/using-a-debugger.md"
---

{{< edit_this_page >}}

## Was & Warum?
Einen Debugger zu verwenden bedeutet, ein Werkzeug zu starten, das es Ihnen ermöglicht, in Ihr laufendes Programm zu schauen, um zu verstehen, was wirklich passiert. Programmierer tun dies, um Fehler zu finden und zu beheben – diese nervigen Probleme, die dazu führen, dass Ihr Code sich unerwartet verhält oder abstürzt.

## Wie geht das:
C++ integriert sich mit Debuggern wie GDB oder dem Visual Studio Debugger. Hier ist ein kurzes Beispiel mit GDB:

```C++
#include <iostream>

int main() {
    int a = 5;
    int b = 0;
    int c = a / b; // Hoppla, Division durch Null!
    std::cout << c << std::endl;
    return 0;
}

// Kompilieren mit:
// g++ -g -o my_program my_program.cpp

// Mit Debugger ausführen:
// gdb ./my_program
```

Sobald Sie GDB gestartet haben, können Sie Haltepunkte setzen, durch Ihren Code schrittweise gehen, Variablen inspizieren und vieles mehr. Wenn Sie das obige Programm ausführen, sollten Sie sehen, dass es aufgrund der Division durch Null abstürzt.

## Vertiefung
Das Debugging hat seine Wurzeln in den Anfängen der Programmierung, wo es buchstäblich notwendig war, Bugs (Insekten!) aus der Hardware zu entfernen. Seitdem haben sich Debugging-Tools zu komplexen und leistungsstarken Softwarelösungen entwickelt, die für die Entwicklung unerlässlich sind.

Alternativen zu GDB für C++ sind LLDB sowie IDE-integrierte Debugger wie die in Visual Studio, CLion oder Eclipse. Diese modernen Umgebungen bieten grafische Schnittstellen, die das Debugging weniger einschüchternd machen.

Die Implementierungsdetails zur Verwendung eines Debuggers hängen oft von Ihrer Entwicklungsumgebung ab:

- Kommandozeilen-Debugger (GDB, LLDB) erfordern Vertrautheit mit Terminalbefehlen und haben oft eine steilere Lernkurve.
- Grafische Debugger vereinfachen den Prozess, indem sie Interaktionen per Klick ermöglichen, um Haltepunkte zu setzen, schrittweise durch den Code zu gehen und Variablen zu beobachten.

Das Verständnis der Fähigkeiten Ihres Debuggers, wie bedingte Haltepunkte, Watchpoints oder das Auswerten von Ausdrücken, kann Ihre Effizienz bei der Diagnose von Problemen erheblich steigern.

## Siehe auch
- [GDB-Dokumentation](https://www.gnu.org/software/gdb/documentation/)
- [LLDB Befehlsdokumentation](https://lldb.llvm.org/use/map.html)
- [Visual Studio Debugger Tutorial](https://docs.microsoft.com/de-de/visualstudio/debugger/debugger-feature-tour)
- [Debuggen mit CLion](https://www.jetbrains.com/help/clion/debugging-code.html)
