---
title:                "C: Sprawdzanie, czy istnieje katalog"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego:

Często w trakcie pisania programów w języku C jest konieczne sprawdzenie, czy dany katalog istnieje. Jest to ważne, ponieważ pozwala uniknąć błędów związanych z próbą dostępu do nieistniejącego katalogu. W tym artykule pokażemy, jak w prosty sposób sprawdzić istnienie katalogu w języku C.

## Jak to zrobić:

Aby sprawdzić, czy dany katalog istnieje, musimy wykorzystać funkcję `opendir()` z biblioteki `dirent.h`. Przykładowy kod wygląda następująco:

```C
#include <stdio.h>
#include <dirent.h>

int main() {
    DIR *dir = opendir("sciezka_do_katalogu");
    if (dir) {
        printf("Katalog istnieje!\n");
        closedir(dir);
    } else {
        printf("Katalog nie istnieje.\n");
    }
    return 0;
}
```

W powyższym przykładzie wykorzystujemy funkcję `opendir()` do otwarcia katalogu o podanej ścieżce. Jeśli funkcja zwróci wartość inna niż `NULL`, oznacza to, że katalog istnieje. Następnie wyświetlamy odpowiedni komunikat i zamykamy katalog funkcją `closedir()`. W przeciwnym wypadku, gdy funkcja `opendir()` zwróci `NULL`, oznacza to, że katalog nie istnieje.

## Głębsze zanurzenie:

Warto zauważyć, że funkcja `opendir()` może zwrócić wartość inna niż `NULL` również wtedy, gdy nie uda się otworzyć katalogu z powodu braku odpowiednich uprawnień lub gdy podano błędną ścieżkę. Dlatego warto także skorzystać z funkcji `errno` z biblioteki `errno.h`, która umożliwia nam sprawdzenie błędu, który wystąpił podczas próby otworzenia katalogu. Przykładowy kod wykorzystujący tę funkcję wygląda następująco:

```C
#include <stdio.h>
#include <dirent.h>
#include <errno.h>

int main() {
    DIR *dir = opendir("sciezka_do_katalogu");
    if (dir == NULL) {
        if (errno == EACCES) {
            printf("Brak uprawnień do otwarcia katalogu.\n");
        } else if (errno == ENOENT) {
            printf("Katalog nie istnieje.\n");
        } else {
            printf("Inny błąd podczas próby otwarcia katalogu.\n");
        }
    } else {
        printf("Katalog istnieje!\n");
        closedir(dir);
    }
    return 0;
}
```

W tym przypadku wykorzystujemy funkcję `errno` do sprawdzenia, jaki konkretnie błąd wystąpił podczas próby otworzenia katalogu. Dzięki temu możemy wyświetlić bardziej szczegółowy komunikat dla użytkownika.

## Zobacz też:

- [Dokumentacja funkcji opendir()](https://linux.die.net/man/3/opendir)
- [Lista kodów błędów systemu Linux](https://www-numi.fnal.gov/offline_software/srt_public_context/WebDocs/Errors/unix_system_errors.html)