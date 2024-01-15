---
title:                "Sprawdzanie istnienia katalogu"
html_title:           "C++: Sprawdzanie istnienia katalogu"
simple_title:         "Sprawdzanie istnienia katalogu"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzanie, czy dany katalog istnieje, jest ważnym aspektem programowania w języku C++, ponieważ pozwala nam na bezpieczne i efektywne zarządzanie plikami i katalogami w naszych programach. Dzięki temu możemy uniknąć niepożądanych błędów i upewnić się, że nasz program działa poprawnie.

## Jak to zrobić

Aby sprawdzić, czy dany katalog istnieje w naszym programie C++, możemy skorzystać z funkcji `std::filesystem::exists()` z biblioteki `<filesystem>`. Jest to nowy sposób na zarządzanie plikami i katalogami w C++ od wersji C++17.

Przykładowy kod wykorzystujący tę funkcję może wyglądać następująco:

```C++
#include <iostream>
#include <filesystem>

int main()
{
    // ścieżka do katalogu, którego istnienie chcemy sprawdzić
    std::filesystem::path directory = "/home/user/Documents/";

    // sprawdzenie, czy katalog istnieje
    bool exists = std::filesystem::exists(directory);

    // wypisanie wyniku
    if (exists) {
        std::cout << "Katalog istnieje\n";
    }
    else {
        std::cout << "Katalog nie istnieje\n";
    }

    return 0;
}
```

Przykładowy wynik dla istniejącego katalogu:

```
Katalog istnieje
```

Przykładowy wynik dla nieistniejącego katalogu:

```
Katalog nie istnieje
```

## Głębsza analiza

Funkcja `std::filesystem::exists()` sprawdza, czy dana ścieżka istnieje w systemie plików. Jeśli tak, zwraca `true`, w przeciwnym razie zwraca `false`. Niemniej jednak, istnieją pewne istotne rzeczy, które warto wiedzieć przy korzystaniu z tej funkcji.

Po pierwsze, funkcja ta nie sprawdza, czy dany plik lub katalog jest dostępny (np. czy użytkownik ma odpowiednie uprawnienia do odczytania pliku), a jedynie czy istnieje. Może to być ważne, jeśli nasz program będzie interakcjonować z plikami i katalogami, do których nie mamy pełnego dostępu.

Po drugie, funkcja ta może prowadzić do pomyłek w przypadku, gdy w systemie plików mamy do czynienia z tzw. linkami symbolicznymi (ang. symbolic links). Linki symboliczne są specjalnymi plikami lub katalogami, które wskazują na inną ścieżkę, a `std::filesystem::exists()` sprawdza właśnie stan ostatniego elementu wskazywanej ścieżki. Może to wprowadzić nas w błąd i wyświetlić nieprawidłowy wynik, dlatego ważne jest, aby mieć to na uwadze podczas korzystania z tej funkcji.

## Zobacz również

- [Dokumentacja std::filesystem::exists()](https://en.cppreference.com/w/cpp/filesystem/exists)
- [Przykłady użycia std::filesystem::exists()](https://www.geeksforgeeks.org/stdfilesystemexists-in-c-examples/)
- [Poradnik: Zarządzanie plikami i katalogami w C++17](https://www.codingame.com/playgrounds/27068/managing-files-using-c-17-filesystem)