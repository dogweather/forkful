---
title:    "C++: Überprüfen, ob ein Verzeichnis existiert"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Warum

Das Überprüfen, ob ein Verzeichnis existiert, ist ein wichtiger Schritt in der Programmierung von C++. Es ermöglicht dem Programm, die Existenz von bestimmten Dateien oder Ordnern zu überprüfen, bevor es versucht, darauf zuzugreifen. Auf diese Weise kann man Fehler vermeiden und das Programm robuster machen.

## Wie man Überprüft, ob ein Verzeichnis existiert

Die einfachste Möglichkeit, die Existenz eines Verzeichnisses zu überprüfen, ist die Verwendung der `std::filesystem`-Bibliothek. Diese Bibliothek ist Teil der C++17-Standardbibliothek und bietet Funktionen zum Arbeiten mit Dateisystemen.

Um zu überprüfen, ob ein Verzeichnis existiert, können wir die `std::filesystem::exists()`-Funktion verwenden und als Argument den Pfad zum Verzeichnis angeben. Zum Beispiel:

```C++
#include <iostream>
#include <filesystem>

int main(){
  std::string dir_path = "pfad/zum/verzeichnis";
  if (std::filesystem::exists(dir_path)){
    std::cout << "Das Verzeichnis existiert." << std::endl;
  } else{
    std::cout << "Das Verzeichnis existiert nicht." << std::endl;
  }

  return 0;
}
```

Die Ausgabe wird je nach Existenz des Verzeichnisses entweder "Das Verzeichnis existiert." oder "Das Verzeichnis existiert nicht." sein.

## Tiefergehende Informationen

Um genauer zu verstehen, wie die `std::filesystem::exists()`-Funktion funktioniert, sollten wir uns auch mit der Konzeption des Dateisystems in C++ befassen. Das Dateisystem wird als ein hierarchischer Baum betrachtet, wobei die Wurzel das Stammverzeichnis eines Laufwerks ist. Jedes Verzeichnis oder jede Datei wird als Knoten im Baum betrachtet und kann Kindknoten haben. In C++ haben Verzeichnisse und Dateien jeweils unterschiedliche Datentypen, daher wird `std::filesystem::exists()` für jedes dieser Datentypen anders implementiert.

Außerdem kann es in manchen Fällen notwendig sein, die Berechtigungen für den Zugriff auf ein Verzeichnis zu überprüfen, bevor man seine Existenz überprüft. Hierfür bietet die `std::filesystem::permissions()`-Funktion Möglichkeiten, die Berechtigungen für ein Verzeichnis zu überprüfen und zu ändern.

## Siehe auch

- [C++ Referenz für std::filesystem::exists()](https://en.cppreference.com/w/cpp/filesystem/exists)
- [C++ Referenz für std::filesystem::permissions()](https://en.cppreference.com/w/cpp/filesystem/permissions)
- [Tutorial: Einführung in std::filesystem (Englisch)](https://www.modernescpp.com/index.php/c-17-filesystem-library-introduction)