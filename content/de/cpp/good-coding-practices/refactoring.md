---
title:                "Refactoring"
aliases:
- de/cpp/refactoring.md
date:                  2024-01-26T01:17:02.547204-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/refactoring.md"
---

{{< edit_this_page >}}

## Was & Warum?

Refactoring ist der Prozess, die interne Struktur eines Computerprogramms zu verändern, ohne sein externes Verhalten zu ändern. Programmierer nehmen dies vor, um ihren Code aufzuräumen, wodurch dieser leichter zu verstehen, zu warten und zu erweitern ist.

## Wie:

Stellen Sie sich vor, Sie haben eine Funktion, die ein wenig zu viel tut, wie diese unhandliche Methode, die ein Objekt initialisiert und gleichzeitig Logging durchführt:

```C++
#include <iostream>

class Widget {
public:
    void init(bool verbose) {
        // Initialisierungslogik
        // ...

        // Ausführliches Logging
        if (verbose) {
            std::cout << "Widget initialisiert!" << std::endl;
        }
    }
};

// Nutzung:
Widget w;
w.init(true);
```

Ausgabe:
```
Widget initialisiert!
```

Das Refactoring in sauberere, fokussiertere Methoden könnte so aussehen:

```C++
#include <iostream>

class Widget {
public:
    void init() {
        // Nur Initialisierungslogik
        // ...
    }

    void logInitialization() const {
        std::cout << "Widget initialisiert!" << std::endl;
    }
};

// Nutzung:
Widget w;
w.init();
w.logInitialization();
```

Diese Änderung hat nicht verändert, was das Programm macht, aber sie macht die `Widget`-Klasse modularer und ihre Benutzung klarer.

## Tiefergehend

Das Konzept des Refactorings, wie wir es heute kennen, hat seine Wurzeln in den Smalltalk-Programmiergemeinschaften der 1980er Jahre und wurde stark durch Martin Fowlers Buch "Refactoring: Improving the Design of Existing Code" aus dem Jahr 1999 popularisiert. Heute ist Refactoring ein fester Bestandteil moderner Softwareentwicklung, integriert in verschiedene Entwicklungsansätze wie Agile und TDD (Test-Driven Development).

Wenn wir über Alternativen zum Refactoring sprechen, bewegen wir uns in das Gebiet des Neuschreibens oder Neugestaltens. Refactoring ist strategisch und inkrementell, während ein Neuschreiben bestehenden Code zugunsten einer neuen Lösung verwerfen kann. Ein Neudesign kann derweil bedeutendere Änderungen beinhalten, einschließlich der Änderung der Funktionalität, was nicht das Ziel des reinen Refactorings ist.

Die Implementierungsdetails zum Refactoring können ziemlich detailliert sein. Es gibt viele „Codegerüche“, die ein Refactoring anregen könnten, wie lange Methoden, große Klassen oder duplizierter Code. Es existieren automatisierte Tools, die beim Refactoring helfen können, wie "Clang-Tidy" für C++, das Probleme erkennen und sogar einige Fixes anwenden kann.

Darüber hinaus erfordert Refactoring eine solide Testreihe, um sicherzustellen, dass die Funktionalität unverändert bleibt. Ohne Tests fliegt man gewissermaßen blind und riskiert Regressionen.

## Siehe auch

Für ein tieferes Verständnis des Refactorings und um weitere Beispiele zu sehen, möchten Sie vielleicht Folgendes überprüfen:

- Martin Fowlers klassischen Text "Refactoring: Improving the Design of Existing Code" für grundlegende Ideen und Strategien.
- Die `Clang-Tidy`-Dokumentation unter https://clang.llvm.org/extra/clang-tidy/ für automatisierte Refactoring-Unterstützung in C++.
- "Working Effectively with Legacy Code" von Michael Feathers, das Techniken bereitstellt, um sicher in Kontext von weniger als perfekten bestehenden Codebasen zu refaktorisieren.
