---
title:                "Schreiben auf Standardfehler"
html_title:           "Arduino: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben auf den Standardfehler ist eine Möglichkeit für Programmierer, Fehlermeldungen und Warnungen auf der Konsole auszugeben. Dadurch können Entwickler schnell und einfach potenzielle Probleme in ihrem Code identifizieren und beheben.

## Wie geht's?
Der folgende Code zeigt, wie man einen Text auf den Standardfehler ausgibt:
```Arduino
#include <stdio.h>
int main() {
  fprintf(stderr, "Hallo Welt!");
}
```
Ausgabe:
```
Hallo Welt!
```

## Tiefere Einblicke
Das Konzept des Schreibens auf den Standardfehler stammt aus der Zeit der UNIX-Betriebssysteme, als die Standardein- und -ausgabe auf die Konsole umgeleitet werden konnten. Heutzutage gibt es auch alternative Methoden, um Nachrichten auf der Konsole auszugeben, wie z.B. die Verwendung von Debugging-Tools oder Logging-Bibliotheken.

Die Implementierung des Schreibens auf den Standardfehler ist in der Programmiersprache C verwendet, daher kann sie auch in anderen Programmiersprachen wie Arduino verwendet werden.

## Siehe auch
- [Die Verwendung von fprintf in C](https://www.tutorialspoint.com/c_standard_library/c_function_fprintf.htm)
- [Debugging-Optionen in der Arduino-IDE](https://www.arduino.cc/en/Guide/Environment#debugging-options)