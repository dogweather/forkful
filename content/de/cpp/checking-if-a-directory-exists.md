---
title:                "Überprüfung, ob ein Verzeichnis vorhanden ist"
html_title:           "C++: Überprüfung, ob ein Verzeichnis vorhanden ist"
simple_title:         "Überprüfung, ob ein Verzeichnis vorhanden ist"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Was & Warum?
Das Überprüfen, ob ein Verzeichnis existiert, ist ein häufiger Schritt in der Programmierung. Es wird verwendet, um sicherzustellen, dass eine Datei oder ein Ordner vorhanden ist, bevor damit weitergearbeitet wird. Dies hilft, Fehler zu vermeiden und eine reibungslose Laufzeit des Programms zu gewährleisten.

# Wie geht es?
```C++
#include <iostream>
#include <filesystem>

int main() {
  std::string path = "/home/username/documents/";
  if (std::filesystem::exists(path)) {
    std::cout << "Das Verzeichnis existiert." << std::endl;
  }
  else {
    std::cout << "Das Verzeichnis existiert nicht." << std::endl;
  }
  return 0;
}
```

### Ausgabe:
```Das Verzeichnis existiert.```

# Tief tauchen
Das Überprüfen der Existenz von Verzeichnissen wurde durch die ständige Weiterentwicklung der Programmiersprachen vereinfacht. Früher mussten Programmierer komplexe Funktionen schreiben, um diese Aufgabe zu erfüllen. Heutzutage bietet die Standardbibliothek von C++ die Funktion ```exists()``` in der ```<filesystem>``` Bibliothek, die diese Aufgabe erleichtert.

Alternativ zur Verwendung der ```exists()``` Funktion können auch Datei-Überprüfungs-Methoden wie ```access()``` oder ```stat()``` verwendet werden. Diese Optionen sind jedoch möglicherweise nicht so ausdrucksstark und haben Einschränkungen in der Anzahl der Verzeichnistiefen, die sie überprüfen können.

Bei der Implementierung der Überprüfung der Verzeichnisexistenz wird ein Systemaufruf verwendet, der den Dateipfad überprüft. Wenn der Pfad stimmt, gibt es ein positives Ergebnis, andernfalls ein negatives.

# Siehe auch
- Dokumentation zu C++ Dateisystembibliothek: https://en.cppreference.com/w/cpp/filesystem
- Beispielcode für das Überprüfen von Verzeichnissen mit C++: https://www.geeksforgeeks.org/check-if-a-directory-exists-in-a-path-in-c-c/
- Tutorials zu grundlegenden Filesystem-Operationen mit C++: https://www.modernescpp.com/index.php/basic-file-operations-in-modern-c/