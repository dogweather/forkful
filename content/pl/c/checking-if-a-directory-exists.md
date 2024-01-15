---
title:                "Sprawdzanie, czy istnieje katalog"
html_title:           "C: Sprawdzanie, czy istnieje katalog"
simple_title:         "Sprawdzanie, czy istnieje katalog"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Gdy piszesz programy w języku C, często potrzebujesz sprawdzać, czy dany katalog istnieje. Jest to ważne, ponieważ pozwala upewnić się, że Twój program będzie działał poprawnie nawet wtedy, gdy użytkownik poda niepoprawną ścieżkę do katalogu.

## Jak to zrobić

Sprawdzenie, czy dany katalog istnieje, jest dość proste w języku C. Wystarczy wykorzystać funkcję `opendir()` i sprawdzić czy zwróciła ona wartość różną od `NULL`. W poniższym przykładzie pokazane jest jak to zrobić:

```C
#include <stdio.h>
#include <sys/types.h>
#include <dirent.h>

int main() {
    DIR *dir = opendir("sciezka/do/katalogu");
    if (!dir) {
        printf("Katalog nie istnieje!\n");
    } else {
        printf("Katalog istnieje!\n");
        closedir(dir);
    }
    return 0;
}
```

**Wyjście:**

```
Katalog istnieje!
```

W powyższym kodzie otwieramy katalog "sciezka/do/katalogu" przy użyciu funkcji `opendir()`. Następnie sprawdzamy czy zwrócona wartość jest równa `NULL`, co oznacza, że katalog nie istnieje. W przeciwnym przypadku, katalog istnieje i możemy na nim wykonać odpowiednie operacje. Pamiętaj, aby zawsze zamknąć otwarty katalog przy użyciu funkcji `closedir()`.

## Deep Dive

Funkcja `opendir()` jest częścią biblioteki standardowej języka C i znajduje się w pliku nagłówkowym `<dirent.h>`. Przyjmuje ona jako argument ścieżkę do katalogu i zwraca wskaźnik do struktury typu `DIR`, która jest wykorzystywana do operacji na katalogu.

Dodatkowo, funkcja `opendir()` może zwrócić wartość `NULL` także w przypadku innych problemów, takich jak brak odpowiednich uprawnień do katalogu czy błędna ścieżka do niego. Dlatego warto dokładnie sprawdzać wynik tej funkcji.

## Zobacz także

Jeżeli chcesz dowiedzieć się więcej o funkcji `opendir()`, możesz przeczytać jej dokumentację na stronie [cplusplus.com](https://www.cplusplus.com/reference/cstdio/opendir/). Polecamy także zapoznanie się z funkcją `mkdir()` do tworzenia nowych katalogów oraz funkcją `readdir()` do odczytywania zawartości katalogu.