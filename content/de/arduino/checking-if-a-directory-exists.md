---
title:                "Überprüfung ob ein Verzeichnis existiert"
html_title:           "Arduino: Überprüfung ob ein Verzeichnis existiert"
simple_title:         "Überprüfung ob ein Verzeichnis existiert"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Was & Warum?
Das Überprüfen, ob ein Verzeichnis existiert, ist eine wichtige Funktion für Programmierer, die es ermöglicht, bestimmte Aktionen basierend auf vorhandenen Dateisystemstrukturen auszuführen.

# Wie geht es?
```Arduino
#include <SD.h>
File dir = SD.open("/Pfad/zum/Verzeichnis");
if (dir) {
  // Verzeichnis existiert
  dir.close();
} else {
  // Verzeichnis existiert nicht
}
```

# Tiefer Einblick
1. Historischer Hintergrund:
Das Überprüfen von Verzeichnissen ist ein wichtiger Teil des Dateisystems und wurde in den frühen Tagen der Computerentwicklung eingeführt.

2. Alternativen:
Es gibt verschiedene Möglichkeiten, um zu überprüfen, ob ein Verzeichnis existiert, wie z.B. die Verwendung von Bibliotheken oder anderen Programmiersprachen.

3. Implementierungsdetails:
Das Überprüfen von Verzeichnissen erfordert den Zugriff auf das Dateisystem des Computers, um nach dem gewünschten Verzeichnis zu suchen.

# Siehe auch
- Arduino SD Library: https://www.arduino.cc/reference/en/libraries/sd/
- W3Schools: https://www.w3schools.com/cpp/cpp_files.asp