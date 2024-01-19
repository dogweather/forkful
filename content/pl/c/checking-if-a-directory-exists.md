---
title:                "Sprawdzanie, czy katalog istnieje"
html_title:           "C: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co to i dlaczego?

Sprawdzanie, czy katalog istnieje, to proces weryfikacji obecności określonej ścieżki katalogu w systemie plików. Programiści robią to, aby zapobiec błędom podczas operacji na plikach i katalogach.

## Jak zrobić:

Poniżej przedstawiam kod w języku C, który sprawdza, czy dany katalog istnieje. 

```C
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

int main() {
    struct stat st = {0};
    if (stat("/path/to/your/directory", &st) == -1) {
        printf("Katalog nie istnieje.\n");
    } else {
        printf("Katalog istnieje.\n");
    }
    return 0;
}
```

Jeżeli katalog istnieje, to wyświetli "Katalog istnieje.", a jeżeli nie - "Katalog nie istnieje.".

## Bunny Dive

Chociaż specyficzna funkcja stat() nie była dostępna w pierwszych wersjach C, podobna funkcjonalność była dostarczana przez różne biblioteki. W nowszych wersjach języka C funkcja stat() została dodana do języka jako część API POSIX.

Co więcej, inny sposób sprawdzenia, czy katalog istnieje, to użycie funkcji opendir() z biblioteki dirent.h, ale ta metoda jest mniej popularna, ponieważ może otworzyć katalog, co nie jest zawsze pożądane.

Podczas używania funkcji stat(), warto pamiętać, że sprawdza ona wszelkiego rodzaju "pliki", nie tylko katalogi. Oznacza to, że może ona zwrócić true, nawet jeśli podana ścieżka jest do pliku, a nie do katalogu. Dlatego ważne jest, aby dodatkowo sprawdzić wartość S_ISDIR(st.st_mode) aby upewnić się, że podana ścieżka jest ścieżką do katalogu.

## Zobacz także

- Dokumentacja funkcji stat(): http://man7.org/linux/man-pages/man2/stat.2.html
- Dokumentacja funkcji opendir(): http://man7.org/linux/man-pages/man3/opendir.3.html
- Szczegóły na temat API POSIX: https://pl.wikipedia.org/wiki/POSIX