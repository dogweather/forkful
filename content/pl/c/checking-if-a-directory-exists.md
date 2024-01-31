---
title:                "Sprawdzanie, czy katalog istnieje"
date:                  2024-01-19
simple_title:         "Sprawdzanie, czy katalog istnieje"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
**Co i dlaczego?**
Sprawdzanie, czy katalog istnieje w C, to po prostu test przed wykonaniem operacji, które wymagają jego obecności. Robimy to, aby uniknąć błędów i zapewnić płynność działania programu.

## How to:
**Jak to zrobić:**
```C
#include <sys/stat.h>
#include <stdio.h>
#include <stdbool.h>

bool does_directory_exist(const char *path) {
    struct stat info;
    
    if(stat(path, &info) != 0)
        return false; // nie można uzyskać dostępu do katalogu
    else if(info.st_mode & S_IFDIR) 
        return true; // jest katalogiem
    return false; // jest czymś innym niż katalog
}

int main() {
    const char *dirPath = "/tmp/example_dir";
    
    if(does_directory_exist(dirPath)) {
        printf("Katalog istnieje.\n");
    } else {
        printf("Katalog nie istnieje.\n");
    }
    
    return 0;
}
```
Sample output:
```
Katalog istnieje.
```
or
```
Katalog nie istnieje.
```

## Deep Dive
**Głębsze spojrzenie:**
Historia sięga funkcji `stat` z Unix'a. `stat` zbiera informacje o pliku używając jego ścieżki. Jeśli funkcja zwraca `0`, plik istnieje; `-1` oznacza problem. Bit `S_IFDIR` z `info.st_mode` pokazuje, czy plik jest katalogiem.

Są alternatywy, np. `opendir` i `readdir` z `dirent.h`, ale `stat` jest prostsze i często w zupełności wystarczające.

Jeśli chcesz tworzyć katalogi, gdy ich nie ma, funkcja `mkdir` z `sys/stat.h` jest przydatna. Wtedy możesz najpierw sprawdzić i stworzyć katalog, jeśli to konieczne.

## See Also
**Zobacz również:**
- [Functions: stat, fstat, lstat (GNU Libc)](https://www.gnu.org/software/libc/manual/html_node/Attribute-Meanings.html)
- [POSIX standard for mkdir()](https://pubs.opengroup.org/onlinepubs/9699919799/functions/mkdir.html)
- [opendir() and readdir() documentation](https://pubs.opengroup.org/onlinepubs/007908799/xsh/readdir.html)
