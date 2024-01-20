---
title:                "Überprüfen, ob ein Verzeichnis existiert"
html_title:           "C++: Überprüfen, ob ein Verzeichnis existiert"
simple_title:         "Überprüfen, ob ein Verzeichnis existiert"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Überprüfen, ob ein Verzeichnis existiert: C++ Programmierung

## Was & Warum?

Das Überprüfen, ob ein Verzeichnis existiert, ist eine Routine im Dateizugriff mannigfaltiger Anwendungen. Dies verhindert Fehler oder Ausnahmen durch nicht vorhandene Pfade.

## So geht's:

In C++ verwenden wir die Bibliothek `<filesystem>`. Im folgenden Beispiel prüfen wir, ob ein Verzeichnis namens "BeispielVerzeichnis" existiert und geben eine entsprechende Nachricht aus.

```C++
#include <filesystem>
#include <iostream>

int main() {
    std::filesystem::path dir("BeispielVerzeichnis");

    if (std::filesystem::exists(dir)) {
        std::cout << "Das Verzeichnis existiert." << std::endl;
    } else {
        std::cout << "Das Verzeichnis existiert nicht." << std::endl;
    }

    return 0;
}
```

Wenn "BeispielVerzeichnis" existiert, erhalten wir:

```C++
Das Verzeichnis existiert.
```

Falls nicht:

```C++
Das Verzeichnis existiert nicht.
```

## Deep Dive

Frühere C++-Versionen boten keine einfache Möglichkeit zur Überprüfung der Existenz von Verzeichnissen. Erst mit C++17 wurde `<filesystem>` eingeführt, das solche Funktionen bietet.

Es gibt auch Alternativen wie die POSIX-Funktion `stat()` oder die Verwendung von Boost-Dateisystemen, aber `<filesystem>` bietet eine deutlich elegantere und einfacher zu handhabende Lösung.

Die Funktion std::filesystem::exists() selbst ruft die Funktion `status()` auf und überprüft auf Fehler.

## Siehe auch

- Offizielle C++ Dokumentation <filesystem> : https://en.cppreference.com/w/cpp/filesystem
- Alternative Wege zum Überprüfen des Verzeichnis: https://stackoverflow.com/questions/4316442/stdofstream-check-if-file-exists-before-writing