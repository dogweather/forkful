---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:56.732375-07:00
description: "Wie geht das: In modernem C++ (C++17 und dar\xFCber hinaus) k\xF6nnen\
  \ Sie die Dateisystembibliothek verwenden, um zu \xFCberpr\xFCfen, ob ein Verzeichnis\
  \ existiert.\u2026"
lastmod: '2024-03-13T22:44:54.198888-06:00'
model: gpt-4-0125-preview
summary: "In modernem C++ (C++17 und dar\xFCber hinaus) k\xF6nnen Sie die Dateisystembibliothek\
  \ verwenden, um zu \xFCberpr\xFCfen, ob ein Verzeichnis existiert."
title: "\xDCberpr\xFCfung, ob ein Verzeichnis existiert"
weight: 20
---

## Wie geht das:
In modernem C++ (C++17 und darüber hinaus) können Sie die Dateisystembibliothek verwenden, um zu überprüfen, ob ein Verzeichnis existiert. Sie bietet eine unkomplizierte und standardisierte Möglichkeit, Dateisystemoperationen durchzuführen, einschließlich der Überprüfung der Existenz eines Verzeichnisses.

```cpp
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main() {
    const fs::path dirPath = "/pfad/zum/verzeichnis";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "Das Verzeichnis existiert." << std::endl;
    } else {
        std::cout << "Das Verzeichnis existiert nicht." << std::endl;
    }

    return 0;
}
```
Beispielausgabe, wenn das Verzeichnis existiert:
```
Das Verzeichnis existiert.
```

Beispielausgabe, wenn das Verzeichnis nicht existiert:
```
Das Verzeichnis existiert nicht.
```

Für Projekte, die noch nicht C++17 verwenden oder zusätzliche Funktionen benötigen, ist die Boost-Dateisystembibliothek eine beliebte Drittanbieteroption, die ähnliche Funktionalitäten bietet.

```cpp
#include <iostream>
#include <boost/filesystem.hpp>

namespace fs = boost::filesystem;

int main() {
    const fs::path dirPath = "/pfad/zum/verzeichnis";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "Das Verzeichnis existiert." << std::endl;
    } else {
        std::cout << "Das Verzeichnis existiert nicht." << std::endl;
    }

    return 0;
}
```
Bei Verwendung von Boost Filesystem wäre die Ausgabe identisch mit dem Beispiel des C++17-Dateisystems, abhängig von der Existenz des Verzeichnisses unter dem angegebenen Pfad.
