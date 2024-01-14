---
title:                "C++: Lesen einer Textdatei"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Du fragst dich vielleicht, warum du diesen Artikel über das Einlesen von Textdateien lesen solltest. Textdateien sind eine häufig verwendete Art der Datenspeicherung und das Einlesen von Textdateien ist eine grundlegende Fähigkeit in der C++ Programmierung. Durch das Verstehen dieses Konzepts kannst du deine Fähigkeiten in der Programmierung erweitern und komplexere Aufgaben bewältigen.

## Wie geht man vor 

Das Einlesen von Textdateien in C++ kann auf verschiedene Arten erfolgen, aber wir werden uns hier auf die Verwendung der <code>ifstream</code> Bibliothek und der <code>getline()</code> Funktion konzentrieren. 

```
#include <iostream>
#include <fstream>

int main() {
    // Öffnen der Textdatei "beispiel.txt" im Lese-Modus
    std::ifstream datei;
    datei.open("beispiel.txt", std::ifstream::in);

    // Überprüfen, ob die Datei korrekt geöffnet wurde
    if (datei.is_open()) {
        // Variable zum Speichern jeder einzelnen Zeile
        std::string zeile;
        
        // Schleife zum Durchlaufen der gesamten Datei zeilenweise
        while (getline(datei, zeile)) {
            // Ausgabe jeder Zeile auf der Konsole
            std::cout << zeile << std::endl;
        }

        // Schließen der Datei nach dem Lesen
        datei.close();
    }
    else {
        // Ausgabe einer Fehlermeldung, falls Datei nicht geöffnet werden kann
        std::cout << "Datei konnte nicht geöffnet werden" << std::endl;
    }
    
    return 0;
}
```

### Beispieleingabe
```
Hallo
Das ist ein Beispieltext
für das Einlesen von Textdateien
in C++
```

### Beispielausgabe
```
Hallo
Das ist ein Beispieltext
für das Einlesen von Textdateien
in C++
```

## Tiefergehende Informationen

Das Einlesen von Textdateien in C++ ist ein grundlegender Prozess, aber es gibt einige interessante Aspekte, die es lohnt sich tiefer damit zu beschäftigen.

### Öffnen von Dateien mit verschiedenen Encoding-Typen

Manchmal werden Textdateien in verschiedenen Encoding-Typen gespeichert, wie z.B. UTF-8 oder UTF-16. Um solche Dateien korrekt einzulesen, kann man den dritten Parameter der <code>open()</code> Funktion verwenden, um den Encoding-Typ anzugeben.

```
// Öffnen einer UTF-8 codierten Datei
datei.open("beispiel.txt", std::ifstream::in, std::ifstream::in | std::ifstream::binary); 
```

### Verwendung der <code>seekg()</code> Funktion

Die <code>seekg()</code> Funktion ermöglicht es, innerhalb einer Textdatei zu navigieren und eine bestimmte Position anzugeben, ab der gelesen werden soll. Dies ist besonders nützlich, wenn man nur einen Teil der Datei einlesen möchte.

```
// Positionieren an der 7. Zeile
datei.seekg(6); 

// Auslesen der nächsten 3 Zeilen
for (int i = 0; i < 3; i++) {
    getline(datei, zeile);
}
```

## Siehe auch

- [Dateien ein- und auslesen in C++](https://www.programiz.com/cpp-programming/files-input-output)
- [C++ Referenz zu iostream und ifstream](https://www.cplusplus.com/reference/iostream/)
- [Verwendung der getline() Funktion](https://www.cplusplus.com/reference/string/string/getline/)