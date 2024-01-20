---
title:                "Überprüfen, ob ein Verzeichnis existiert"
html_title:           "Arduino: Überprüfen, ob ein Verzeichnis existiert"
simple_title:         "Überprüfen, ob ein Verzeichnis existiert"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?

In C++ prüfen wir, ob ein Verzeichnis existiert, um sicherzustellen, dass Dateioperationen nicht fehlschlagen. Das ist ein alltäglicher Check, der Datenverlust verhindert und die Benutzerfreundlichkeit sicherstellt.

## Wie geht das?

```C++
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main() {
    fs::path dir = "/path/to/directory";

    if (fs::exists(dir)) {
        std::cout << "Das Verzeichnis existiert!" << std::endl;
    } else {
        std::cout << "Das Verzeichnis existiert nicht." << std::endl;
    }

    return 0;
}
```

Ausgabe je nach Verzeichnisstatus:
```
Das Verzeichnis existiert!
```
oder
```
Das Verzeichnis existiert nicht.
```

## Tiefgang

Früher mussten wir auf Betriebssystem-spezifische APIs zurückgreifen, um die Existenz eines Verzeichnisses zu prüfen. In C++ gab es keine standardisierte Weg dafür bis zur Einführung des Filesystem Libraries in C++17. Jetzt benutzen wir std::filesystem, das plattformunabhängig ist.

Alternativen wie Boost.Filesystem existierten vor std::filesystem und werden immer noch verwendet, falls Kompatibilität mit älteren C++-Standards benötigt wird oder erweiterte Funktionen gebraucht sind. 

Das std::filesystem::exists liefert uns einfach `true` oder `false` zurück. Für zusätzliche Information, wie etwa Zugriffsrechte, müssen wir weiterführende Funktionen wie `fs::status` verwenden.

## Siehe auch

- C++ Filesystem Library Dokumentation: https://en.cppreference.com/w/cpp/filesystem
- Artikel über Portabilität von Filesystem Operationen in C++: http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2014/n4100.pdf
- Boost.Filesystem Dokumentation: https://www.boost.org/doc/libs/release/libs/filesystem/