---
title:    "C: Überprüfen, ob ein Ordner vorhanden ist"
keywords: ["C"]
---

{{< edit_this_page >}}

# Warum?

Das Überprüfen, ob ein Verzeichnis existiert, ist wichtig, um sicherzustellen, dass unsere Programme effizient und zuverlässig funktionieren. Es hilft uns, unerwartete Fehler zu vermeiden und unsere Code-Basis zu schützen.

# Wie geht man vor?

Um zu überprüfen, ob ein Verzeichnis in C existiert, müssen wir die "dirent.h" Header-Datei einbinden und die Funktion "opendir()" verwenden. Diese Funktion öffnet das Verzeichnis und gibt einen Zeiger darauf zurück, falls es existiert. Falls nicht, wird ein NULL-Zeiger zurückgegeben.

```C
#include <stdio.h>
#include <dirent.h>

int main() {
    // Verzeichnis öffnen
    DIR *dir = opendir("beispielverzeichnis");
    // Überprüfen, ob NULL-Zeiger zurückgegeben wurde (Verzeichnis existiert nicht)
    if(dir == NULL) {
        printf("Das Verzeichnis existiert nicht \n");
    }
    else {
        printf("Das Verzeichnis existiert \n");
    }
    return 0;
}
```

Wenn das Verzeichnis existiert, wird "Das Verzeichnis existiert" ausgegeben, ansonsten "Das Verzeichnis existiert nicht".

# Tiefere Einblicke

Die Funktion "opendir()" kann auch verwendet werden, um die Dateien und Verzeichnisse innerhalb des angegebenen Verzeichnisses zu durchlaufen. Dazu kann die Funktion "readdir()" verwendet werden, die einen Zeiger auf die nächste Datei im Verzeichnis zurückgibt.

```C
#include <stdio.h>
#include <dirent.h>

int main() {
    // Verzeichnis öffnen
    DIR *dir = opendir("beispielverzeichnis");
    // Solange es eine nächste Datei gibt
    while ((dirent = readdir(dir)) != NULL) {
        // Dateinamen ausgeben
        printf("%s\n", dirent->d_name);
    }
    return 0;
}
```

Mit dieser Methode können wir alle Dateien und Verzeichnisse in einem bestimmten Verzeichnis durchlaufen und damit weitere Aktionen wie das Löschen oder Verschieben von Dateien durchführen.

# Siehe auch

- [Direktiven und Makros für Verzeichnis-Operationen](https://www.gnu.org/software/libc/manual/html_node/Directory-Operatives.html)
- [Verzeichnisfunktionen in C](https://www.tutorialspoint.com/c_standard_library/directory_h.htm)