---
title:                "C: Sprawdzanie istnienia katalogu"
simple_title:         "Sprawdzanie istnienia katalogu"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Niektórzy z nas mogą zadać sobie pytanie, po co w ogóle sprawdzać istnienie danego katalogu w naszym programie w C? Jednym z głównych powodów jest zapewnienie bezpieczeństwa naszego kodu. Dzięki temu, możemy upewnić się, że nasze operacje plikowe będą działać poprawnie w przypadku, gdy dany katalog nie istnieje.

## Jak to zrobić

Istnieje kilka sposobów na sprawdzenie, czy dany katalog istnieje w języku programowania C. Poniżej przedstawione są przykładowe kody, które wskazują, jak to zrobić.

Przy użyciu funkcji `opendir()`:

```C
#include <stdio.h>
#include <dirent.h>

int main()
{
    char* path = "/home/user/Desktop"; // ścieżka do katalogu, którego istnienie chcemy sprawdzić

    DIR* dir = opendir(path); // otwarcie katalogu

    if(dir) // jeśli katalog istnieje
    {
        printf("%s directory exists.\n", path);
        closedir(dir); // zamknięcie katalogu
    }
    else // jeśli katalog nie istnieje
    {
        printf("%s directory does not exist.\n", path);
    }
    
    return 0;
}
```

Przy użyciu funkcji `stat()`:

```C
#include <stdio.h>
#include <sys/stat.h>

int main()
{
    char* path = "/home/user/Desktop"; // ścieżka do katalogu, którego istnienie chcemy sprawdzić

    struct stat st = {0}; // tworzenie struktury stat
    int result = stat(path, &st); // wywołanie funkcji stat

    if(result == 0) // jeśli katalog istnieje
    {
        printf("%s directory exists.\n", path);
    }
    else // jeśli katalog nie istnieje
    {
        printf("%s directory does not exist.\n", path);
    }
    
    return 0;
}
```

## Głębsza analiza

W powyższych przykładach, użytkownik musi podać ścieżkę do katalogu, który chce sprawdzić. Jest to najprostsza i najbardziej powszechna metoda. Jednak, istnieją również inne sposoby, takie jak użycie funkcji `access()` lub `realpath()`. Najważniejsze jest upewnienie się, że używamy odpowiednich funkcji i odpowiednio obsługujemy potencjalne błędy.

## Zobacz również

- [Dokumentacja funkcji opendir()](https://www.man7.org/linux/man-pages/man3/opendir.3.html)
- [Dokumentacja funkcji stat()](https://www.man7.org/linux/man-pages/man2/stat.2.html)
- [Poradnik na temat funkcji access()](https://www.dynatrace.com/resources/ebooks/javabook/access-and-check-for-existence-of-files-directories/)
- [Przykładowe kody na GitHubie](https://github.com/topics/directory-exists-c)