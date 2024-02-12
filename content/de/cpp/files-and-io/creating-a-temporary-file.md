---
title:                "Erstellung einer temporären Datei"
aliases:
- /de/cpp/creating-a-temporary-file/
date:                  2024-01-20T17:39:46.700926-07:00
model:                 gpt-4-1106-preview
simple_title:         "Erstellung einer temporären Datei"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Ein temporäres File ist eine kurzlebige Datei, die während der Ausführung eines Programms erstellt und oft danach gelöscht wird. Programmierer nutzen sie für Datenhaltung, die nur während einer Session gebraucht wird – etwa für Zwischenergebnisse oder als Backup.

## How to:
```C++
#include <cstdio>     // Für tmpfile() und fclose()
#include <fstream>    // Für std::ofstream
#include <iostream>   // Für std::cout
#include <filesystem> // Für std::filesystem::temp_directory_path()

int main() {
    // Methode 1: tmpfile()
    FILE* tempPtr = tmpfile();
    if (!tempPtr) {
        std::cerr << "Temporäres File konnte nicht erstellt werden.\n";
        return 1;
    }

    // Nutzen Sie das File ...
    
    // Das File wird automatisch gelöscht, wenn es geschlossen wird
    fclose(tempPtr);

    // Methode 2: C++ Dateistream
    std::ofstream tempFile(std::filesystem::temp_directory_path() / "meinTempFile.txt");
    if (!tempFile.is_open()) {
        std::cerr << "Temporäres File konnte nicht erstellt werden.\n";
        return 1;
    }

    // Nutzen Sie das File ...

    // Schließen und manuell löschen
    tempFile.close();
    std::filesystem::remove(std::filesystem::temp_directory_path() / "meinTempFile.txt");

    std::cout << "Temporäre Files wurden erstellt und gelöscht.\n";
    return 0;
}
```
**Ausgabe:**
```
Temporäre Files wurden erstellt und gelöscht.
```

## Deep Dive:
Frühe Betriebssysteme boten einfache Methoden, temporäre Files zu handhaben. Heutige C++-Standards nutzen Funktionen wie `tmpfile()` oder Streams, um Portabilität und Sicherheit zu gewährleisten. Alternativen wie Named Temporary Files erlauben mehr Kontrolle, bergen aber Risiken hinsichtlich Kollisionen und Sicherheit. Bei der Implementierung sollten Entwickler sorgfältig überlegen, ob die Daten sensibel sind und wie sie das File löschen, da temporäre Files ein Sicherheitsrisiko darstellen können.

## See Also:
- CPP Reference für `tmpfile()`: https://en.cppreference.com/w/c/io/tmpfile
- CPP Reference für `std::filesystem`: https://en.cppreference.com/w/cpp/filesystem
- Eine gute Diskussion über die Sicherheit von temporären Files: https://owasp.org/www-community/vulnerabilities/Insecure_Temporary_File
