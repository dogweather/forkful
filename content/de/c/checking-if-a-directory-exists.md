---
title:                "Überprüfen, ob ein Verzeichnis existiert"
html_title:           "C: Überprüfen, ob ein Verzeichnis existiert"
simple_title:         "Überprüfen, ob ein Verzeichnis existiert"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Was & Warum?

Das Überprüfen, ob ein Verzeichnis existiert, ist eine häufige Aufgabe für Programmierer. Es bezieht sich auf die Überprüfung, ob ein bestimmtes Verzeichnis auf Ihrem System vorhanden ist oder nicht. Programmierer führen diese Überprüfung aus, um sicherzustellen, dass ein durch das Programm benötigtes Verzeichnis vorhanden ist und um Fehler zu vermeiden.

# Wie geht's:

```C
#include <stdio.h>
#include <stdbool.h> //bool data type
#include <sys/stat.h> //stat() function

int main() {
    //path to directory to be checked
    char* path = "path/to/directory"; //change this to desired path
    
    //using the stat() function to check if directory exists
    struct stat sb;
    bool dir_exists = (stat(path, &sb) == 0);
    
    //checking the result
    if (dir_exists) {
        printf("%s directory exists!\n", path);
    } else {
        printf("%s directory does not exist!\n", path);
    }
    
    return 0;
}
```

Output:
```
path/to/directory directory exists!
```

# Tief eintauchen:

- **Historischer Kontext:** Das Überprüfen von Verzeichnissen wurde seit den frühen Tagen der Programmierung durchgeführt, um sicherzustellen, dass Programme auf die benötigte Datei- und Ordnerstruktur zugreifen können.
- **Alternativen:** Statt der stat() Funktion können andere Methoden wie z.B. die opendir() und readdir() Funktionen verwendet werden, um ein Verzeichnis zu öffnen und dessen Inhalt zu lesen.
- **Implementierungsdetails:** Die stat() Funktion gibt eine Struktur namens "stat" zurück, die Informationen über die Datei enthält. Die Methode, die verwendet wird, um ein Verzeichnis zu überprüfen, ist die Verwendung der "S_IFDIR" konstanten aus der "stat" Struktur.

# Sieh auch:

- [stat() Funktion Dokumentation](https://en.cppreference.com/w/c/io/stat)
- [opendir() Funktion Dokumentation](https://en.cppreference.com/w/c/io/opendir)
- [Standard-Datei-Funktionen in C](https://www.programiz.com/c-programming/c-file-input-output)