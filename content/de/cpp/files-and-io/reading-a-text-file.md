---
aliases:
- /de/cpp/reading-a-text-file/
date: 2024-01-20 17:54:00.900171-07:00
description: "Das Lesen einer Textdatei in C++ bedeutet, dass wir den Inhalt einer\
  \ Datei in unserem Programm verwenden. Wir machen das, um Daten zu verarbeiten,\
  \ die in\u2026"
lastmod: 2024-02-18 23:09:05.207220
model: gpt-4-1106-preview
summary: "Das Lesen einer Textdatei in C++ bedeutet, dass wir den Inhalt einer Datei\
  \ in unserem Programm verwenden. Wir machen das, um Daten zu verarbeiten, die in\u2026"
title: Textdatei einlesen
---

{{< edit_this_page >}}

## Was & Warum?
Das Lesen einer Textdatei in C++ bedeutet, dass wir den Inhalt einer Datei in unserem Programm verwenden. Wir machen das, um Daten zu verarbeiten, die in Dateien gespeichert sind, sei es Konfiguration, Benutzereingaben oder Log-Daten.

## How to:
Ein einfaches Beispiel, um eine Textdatei Zeile für Zeile zu lesen:

```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream file("Beispiel.txt");
    std::string line;
    
    if (file.is_open()) {
        while (std::getline(file, line)) {
            std::cout << line << '\n';
        }
        file.close();
    } else {
        std::cout << "Datei konnte nicht geöffnet werden." << std::endl;
    }

    return 0;
}
```

Angenommen, `Beispiel.txt` enthält:

```
Hallo Welt!
Das ist eine Textdatei.
Auf Wiedersehen!
```

Die Ausgabe wäre:

```
Hallo Welt!
Das ist eine Textdatei.
Auf Wiedersehen!
```

## Deep Dive:
Das Lesen von Textdateien ist grundlegend und relevant, seitdem Dateisysteme existieren. Historisch bedienten sich frühe C++-Programmierer oft der C-Standardbibliothek (`<stdio.h>`) mit `fopen`, `fgets` und `fclose`. Mit der Einführung der C++-Standardbibliothek bot sich der `std::ifstream` als bequemere, objektorientierte Möglichkeit an.

Alternativen zum Datei-Lesen in C++ können sein: `mmap` (Mapping der Datei ins Speicher), die Nutzung von Bibliotheken wie Boost.IOStreams oder moderne C++17-Dateisystem-Libraries (`<filesystem>`).

Wichtig bei der Implementierung ist das korrekte Handling von Fehlern, wie zum Beispiel die Datei existiert nicht, und Ressourcen, insbesondere das Schließen von Datei-Handles (`file.close()`). Moderne C++ Praktiken nutzen RAII (Resource Acquisition Is Initialization), also Klassen die Ressourcen in Konstruktoren erwerben und im Destruktor freigeben, um sicherzustellen, dass die Dateien korrekt geschlossen werden.

## See Also:
- C++-Referenz für `<fstream>`: https://en.cppreference.com/w/cpp/io/basic_fstream
- Ein Tutorial zur modernen C++-Dateiverwaltung: https://www.learncpp.com/cpp-tutorial/186-basic-file-io/
- Offizielle Dokumentation zum `<filesystem>`-Modul: https://en.cppreference.com/w/cpp/filesystem
