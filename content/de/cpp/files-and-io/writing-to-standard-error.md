---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:06.571692-07:00
description: "Das Schreiben auf den Standardfehler (`stderr`) in C++ beinhaltet die\
  \ Ausgabe von Fehlermeldungen oder Diagnosen, die getrennt von der\u2026"
lastmod: '2024-03-13T22:44:54.200822-06:00'
model: gpt-4-0125-preview
summary: "Das Schreiben auf den Standardfehler (`stderr`) in C++ beinhaltet die Ausgabe\
  \ von Fehlermeldungen oder Diagnosen, die getrennt von der\u2026"
title: Schreiben auf Standardfehler
weight: 25
---

## Was & Warum?

Das Schreiben auf den Standardfehler (`stderr`) in C++ beinhaltet die Ausgabe von Fehlermeldungen oder Diagnosen, die getrennt von der Hauptprogrammausgabe sind. Programmierer tun dies, um Fehler auf einen anderen Stream umzuleiten, was das Debugging und die Fehlerbehandlung durch die Unterscheidung zwischen normaler Ausgabe und Fehlermeldungen erleichtert.

## Wie geht das:

In C++ kann das Schreiben auf den Standardfehler mit dem `cerr`-Stream erreicht werden, der Teil der Standardbibliothek ist. Hier ist ein einfaches Beispiel:

```cpp
#include <iostream>

int main() {
    // Schreiben auf die Standardausgabe
    std::cout << "Das ist eine normale Nachricht." << std::endl;
    
    // Schreiben auf den Standardfehler
    std::cerr << "Das ist eine Fehlermeldung." << std::endl;
    
    return 0;
}
```

Beispiel Ausgabe:
```
Das ist eine normale Nachricht.
Das ist eine Fehlermeldung.
```

In diesem Fall werden beide Nachrichten typischerweise auf Ihrem Terminal erscheinen, aber Sie können sie in einer Shell separat umleiten. Zum Beispiel können Sie die Standardausgabe in eine Datei senden, während Fehler auf dem Bildschirm angezeigt werden.

Für fortgeschrittenere Logging- und Fehlerbehandlung können Drittanbieter-Bibliotheken wie `spdlog` oder `boost.log` verwendet werden. Diese Bibliotheken bieten erweiterte Funktionen für das Logging, einschließlich Formatierung, Log-Level und Dateiausgabe.

So könnten Sie `spdlog` verwenden, um eine Fehlermeldung zu schreiben:

```cpp
#include "spdlog/spdlog.h"

int main() {
    // Initialisiere spdlog
    spdlog::info("Das ist eine normale Nachricht.");
    spdlog::error("Das ist eine Fehlermeldung.");
    
    return 0;
}
```

Hinweis: Um `spdlog` zu verwenden, müssen Sie es zu Ihrem Projekt hinzufügen. Dies können Sie tun, indem Sie das Repository von GitHub klonen oder einen Paketmanager wie `vcpkg` oder `conan` verwenden.

Denken Sie daran, die Wahl zwischen der direkten Verwendung von Standardströmen oder einer Bibliothek wie `spdlog` hängt von der Komplexität Ihrer Anwendung und Ihren spezifischen Bedürfnissen in Bezug auf Fehlerbehandlung und Logging ab.
