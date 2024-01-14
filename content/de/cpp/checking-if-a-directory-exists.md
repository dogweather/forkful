---
title:                "C++: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Das Überprüfen, ob ein Verzeichnis vorhanden ist, ist ein wichtiger Schritt bei der Programmierung von Anwendungen, die mit Dateiverwaltung zu tun haben, wie z.B. Dateiexplorern oder Backup-Programmen. Durch die Überprüfung kann sichergestellt werden, dass die Anwendung richtig funktioniert und keine Fehler auftreten.

## Wie geht es

Eine Möglichkeit, um zu überprüfen, ob ein Verzeichnis vorhanden ist, ist die Verwendung von C++'s `std::filesystem`-Bibliothek. Diese Bibliothek bietet Funktionen und Klassen für die Dateiverwaltung, einschließlich der Überprüfung von Verzeichnissen. Im Folgenden ist ein Beispielcode aufgeführt, der die Funktion `std::filesystem::exists()` verwendet, um zu überprüfen, ob ein Verzeichnis mit dem angegebenen Pfad vorhanden ist:

```C++
#include <iostream>
#include <filesystem>

int main() {
    // Definiere den Pfad des zu überprüfenden Verzeichnisses
    std::filesystem::path myDir = "C:/Users/Username/Desktop/myFolder"; 

	// Überprüfe, ob das Verzeichnis vorhanden ist
	if (std::filesystem::exists(myDir)) {
		std::cout << "Das Verzeichnis existiert.";
	} else {
		std::cout << "Das Verzeichnis wurde nicht gefunden.";
	}

	return 0;
}
```

Beispiel Ausgabe:

```
Das Verzeichnis existiert.
```

## Tiefer tauchen

Der oben genannte Code verwendet die `std::filesystem::exists()`-Funktion, um zu überprüfen, ob ein Verzeichnis vorhanden ist. Diese Funktion gibt einen booleschen Wert zurück (`true` oder `false`), abhängig davon, ob das Verzeichnis vorhanden ist oder nicht. Es gibt auch andere Funktionen in der `std::filesystem`-Bibliothek, wie z.B. `std::filesystem::is_directory()` und `std::filesystem::is_regular_file()`, die verwendet werden können, um spezifischere Überprüfungen durchzuführen.

Es ist auch wichtig zu beachten, dass die `std::filesystem`-Bibliothek Teil des C++17-Standards ist. Um sicherzustellen, dass Ihre Anwendung auf allen Plattformen funktioniert, sollte geprüft werden, ob die Bibliothek auf dem verwendeten System verfügbar ist.

## Siehe auch

- [C++-Referenz für std::filesystem](https://en.cppreference.com/w/cpp/filesystem)
- [Überprüfen eines Verzeichnisses mit Boost.Filesystem](https://www.boost.org/doc/libs/master/libs/filesystem/doc/index.htm)
- [Dateien und Verzeichnisse in C++ verwalten](https://www.techopedia.com/2/29170/software/development/a-primer-on-file-and-directory-management-in-c)