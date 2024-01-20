---
title:                "Schreiben auf Standardfehler"
html_title:           "Arduino: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Standardfehlerausgabe (`stderr`) ist ein separater Ausgabekanal für Fehlermeldungen und Diagnoseinformationen. Programmierer nutzen diese, um Fehler und wichtige Warnungen unabhängig von der normalen Ausgabe (`stdout`) auszugeben – damit bleiben Log-Dateien klar und Wartung wird erleichtert.

## How to:
### Fehler an `stderr` senden:
```C++
#include <iostream>

int main() {
    std::cerr << "Fehler: Operation konnte nicht abgeschlossen werden!" << std::endl;
    return 0;
}
```
### Möglicher Output:
```
Fehler: Operation konnte nicht abgeschlossen werden!
```
### Unterschiedliche Ausgaben in `stdout` und `stderr`:
```C++
#include <iostream>

int main() {
    std::cout << "Standardausgabe" << std::endl;
    std::cerr << "Fehlermeldung" << std::endl;
    return 0;
}
```
### Möglicher Output:
```
Standardausgabe
Fehlermeldung
```
(Achtung: Die Reihenfolge kann variieren, weil `stdout` gepuffert ist, `stderr` jedoch meist nicht.)

## Deep Dive:
### Historischer Kontext:
Unix-basierte Systeme leiteten die Konvention ein, Fehler oder Diagnoseinformationen über einen anderen Stream (`stderr`) als normale Daten (`stdout`) auszugeben. Das ermöglichte eine effektivere Verarbeitung und Analyse von Fehlermeldungen.

### Alternativen:
Neben der Verwendung von `std::cerr` können auch eigene Logging-Frameworks oder Systeme wie `syslog` unter Linux bzw. `Event Tracing` unter Windows eingesetzt werden, die mehr Kontrolle und Flexibilität bieten.

### Implementierungsdetails:
`std::cerr` ist ein globales Objekt von `std::ostream`. Es ist speziell für die Ausgabe von Fehlermeldungen konfiguriert und in der Regel nicht gepuffert, sodass Nachrichten sofort ausgegeben werden.

## See Also:
- C++ Standard Library: [`<iostream>`](http://www.cplusplus.com/reference/iostream/)
- C++ Error Handling: [`<stdexcept>`](http://www.cplusplus.com/reference/stdexcept/)
- Weiterführende Artikel zum Logging und Fehlermanagement in C++.