---
title:                "C: Tworzenie tymczasowego pliku"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Dlaczego tworzymy pliki tymczasowe?

Tworzenie plików tymczasowych może być niezbędnym elementem w wielu programach. Na przykład, gdy potrzebujemy przechować tymczasowe dane, zapisywać pliki tymczasowe lub wykonujemy różne operacje na danych, tworzenie plików tymczasowych jest koniecznym krokiem. W tym artykule dowiesz się dlaczego tworzenie plików tymczasowych jest ważne i jak to zrobić w języku C.

## Jak to zrobić?

W języku C istnieje kilka sposobów na tworzenie plików tymczasowych. Jednym z najpopularniejszych jest użycie funkcji `tmpfile()` lub `mkstemp()`. Oto przykładowy kod, który pokazuje, jak użyć funkcji `tmpfile()` do tworzenia pliku tymczasowego:

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *temp_file = tmpfile();
    if (temp_file == NULL) {
        perror("Nie udało się utworzyć pliku tymczasowego");
        exit(EXIT_FAILURE);
    } else {
        printf("Plik tymczasowy utworzony pomyślnie\n");
        // dalsze operacje na pliku
    }
    
    return 0;
}
```

Output:
```
Plik tymczasowy utworzony pomyślnie
```

Funkcji `mkstemp()` natomiast możemy użyć w ten sposób:

```C
#include <stdio.h>

int main() {
    char template[] = "/tmp/tempfileXXXXXX";
    int fd = mkstemp(template);
    if (fd == -1) {
        perror("Nie udało się utworzyć pliku tymczasowego");
        exit(EXIT_FAILURE);
    } else {
        printf("Plik tymczasowy utworzony pomyślnie\n");
        // dalsze operacje na pliku
    }
    
    return 0;
}
```

Output:
```
Plik tymczasowy utworzony pomyślnie
```

## Głębszy przegląd

Tworzone pliki tymczasowe są usuwane automatycznie po zamknięciu programu. Są one również unikalne - nie mogą istnieć dwa pliki tymczasowe o tej samej nazwie. Jednak niektóre systemy operacyjne mogą mieć różne sposoby tworzenia plików tymczasowych. Na przykład, na systemie Linux, możesz użyć funkcji `mkstemp()` zamiast `tmpfile()` dla większej kontroli nad plikami tymczasowymi.

## Zobacz także

- [Tworzenie plików tymczasowych w języku C na stronie Tutorialspoint](https://www.tutorialspoint.com/c_standard_library/c_function_tmpfile.htm)
- [Dokumentacja funkcji tmpfile() w języku C](https://www.gnu.org/software/libc/manual/html_node/Creating-Temp-Files.html)
- [Porównanie funkcji tmpfile() i mkstemp() w artykule na Medium](https://medium.com/@xrom/libc-tmpfile-vs-mkstemp-395a85bfcac7)