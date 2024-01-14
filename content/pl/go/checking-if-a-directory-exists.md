---
title:    "Go: Sprawdzanie czy istnieje katalog"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzanie, czy istnieje katalog, to ważna część programowania w Go. Bez tego niektóre operacje, takie jak tworzenie nowych plików czy otwieranie istniejących, nie będą działać poprawnie. Dlatego warto poznać mechanizm i sposoby na sprawdzenie, czy dany katalog istnieje.

## Jak to zrobić

Kodowanie w Go jest bardzo proste i intuicyjne, dlatego też sprawdzanie istnienia katalogu jest łatwe. Wystarczy użyć funkcji `os.Stat()` i podać jako argument ścieżkę do katalogu, który chcemy sprawdzić.

```Go
fi, err := os.Stat("ścieżka_do_katalogu")
if err != nil {
    if os.IsNotExist(err) {
        // Katalog nie istnieje
    } else {
        // Błąd podczas sprawdzania
    }
} else {
    // Katalog istnieje
}
```

Jeśli funkcja `os.Stat()` zwróci błąd, możemy skorzystać z funkcji `os.IsNotExist()` aby sprawdzić, czy katalog nie istnieje, a w przypadku innego błędu, obsłużyć go odpowiednio. W przeciwnym razie, możemy wykonać kod dla przypadku gdy katalog istnieje.

## Głębsze wgląd

Warto zauważyć, że funkcja `os.Stat()` zwróci błąd nie tylko w przypadku gdy katalog nie istnieje, ale również w przypadku gdy nie mamy dostępu do danego katalogu. W takiej sytuacji porównanie błędu z `os.IsNotExist()` może dać mylne wyniki. Dlatego też warto w takim przypadku wykorzystać funkcję `os.IsPermission()` aby sprawdzić, czy to tylko brak dostępu, a nie brak istnienia.

Kolejną ważną informacją jest fakt, że funkcja `os.Stat()` zwraca także informacje o tym, czy dany plik jest katalogiem czy nie. Możemy to wykorzystać, aby w razie potrzeby wykonać inny kod dla plików i katalogów.

## Zobacz także

- Dokumentacja funkcji `os.Stat()` w języku angielskim: https://golang.org/pkg/os/#Stat
- Inne przydatne informacje dotyczące operacji na plikach i katalogach w Go: https://medium.com/better-programming/manipulating-files-and-folders-in-golang-5def5b441cd2
- Przykładowe projekty w języku Go na GitHubie: https://github.com/search?q=go