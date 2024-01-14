---
title:                "C: Überprüfen, ob ein Verzeichnis existiert"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

#Warum

Als Programmierer ist es oft notwendig, zu überprüfen, ob ein bestimmtes Verzeichnis vorhanden ist, bevor man versucht, auf Dateien oder Daten innerhalb dieses Verzeichnisses zuzugreifen. Dies ist wichtig, um sicherzustellen, dass das Programm ordnungsgemäß funktioniert und keine unerwünschten Fehler verursacht.

#Wie überprüft man, ob ein Verzeichnis existiert

Die einfachste Möglichkeit, dies in C zu tun, ist die Verwendung der Funktion "opendir()" zusammen mit einer Bedingung, um zu überprüfen, ob das Verzeichnis erfolgreich geöffnet wurde. Hier ist ein Beispielcode:

```
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <dirent.h>

int main() {
    DIR *dir = opendir("mydirectory");
    
    if (dir) {
        // Verzeichnis existiert
        printf("Verzeichnis existiert\n");
        closedir(dir);
    } else if (ENOENT == errno) {
        // Verzeichnis existiert nicht
        printf("Verzeichnis existiert nicht\n");
        // Hier können Aktionen ausgeführt werden, um das Verzeichnis zu erstellen
    } else {
        // Ein anderer Fehler ist aufgetreten
        printf("Fehler beim Öffnen des Verzeichnisses\n");
    }
    return 0;
}
```

Beim ersten Ausführen dieses Codes wird "Verzeichnis existiert nicht" ausgegeben, da das Verzeichnis "mydirectory" noch nicht existiert. Wenn Sie dann das Verzeichnis erstellen und den Code erneut ausführen, wird "Verzeichnis existiert" ausgegeben.

#Tiefere Einblicke

Wenn Sie es vorziehen, eine Funktion zu verwenden, die explizit überprüft, ob ein Verzeichnis vorhanden ist, können Sie die Funktion "access()" verwenden, die in der "unistd.h" Bibliothek definiert ist. Diese Funktion überprüft, ob auf ein bestimmtes Verzeichnis zugegriffen werden kann, indem sie die Berechtigungen überprüft. Wenn das Verzeichnis vorhanden ist und die Zugriffsrechte richtig gesetzt sind, wird die Funktion "access()" "0" zurückgeben. Andernfalls wird sie "-1" zurückgeben.

Hier ist ein Beispielcode:

```
#include <stdio.h>
#include <unistd.h>

int main() {
    // Beispiel für ein vorhandenes Verzeichnis
    if (access("mydirectory", 0) != -1) {
        printf("Verzeichnis existiert\n");
    } else {
        printf("Verzeichnis existiert nicht\n");
    }
    
    // Beispiel für ein nicht vorhandenes Verzeichnis
    if (access("nonexistentdir", 0) != -1) {
        printf("Verzeichnis existiert\n");
    } else {
        printf("Verzeichnis existiert nicht\n");
    }
    return 0;
}
```

Dieser Code gibt "Verzeichnis existiert" für "mydirectory" und "Verzeichnis existiert nicht" für "nonexistentdir" aus.

#Siehe auch

- [Liste der C Bibliotheken](https://de.wikipedia.org/wiki/Liste_von_C-Bibliotheken)
- [Verzeichnis erstellen in C](https://www.educative.io/edpresso/how-to-create-a-directory-in-c)
- [Übersicht über Datei- und Verzeichnisoperationen in C](https://dev.to/victoria/overview-of-file-and-directory-operations-in-c-l8b)