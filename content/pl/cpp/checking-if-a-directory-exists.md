---
title:                "C++: Sprawdzanie czy istnieje katalog"
simple_title:         "Sprawdzanie czy istnieje katalog"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

 Sprawdzanie, czy dany katalog istnieje, jest ważnym aspektem programowania w C++. W wielu przypadkach może to pomóc w uniknięciu błędów lub dostosowaniu działania programu w zależności od istnienia lub braku danego katalogu.

## Jak to zrobić

Aby sprawdzić istnienie katalogu w C++, należy wykorzystać funkcję `std::filesystem::exists()`. Przykładowy kod wygląda następująco:

```C++
#include <iostream>
#include <filesystem>

int main() {
    // ustawienie ścieżki katalogu, który chcemy sprawdzić
    std::filesystem::path directory_path("C:/Users/User/Desktop/katalog");

    // sprawdzenie, czy katalog istnieje
    if(std::filesystem::exists(directory_path)) {
        std::cout << "Katalog istnieje" << std::endl;
    } else {
        std::cout << "Katalog nie istnieje" << std::endl;
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

## Deep Dive

Funkcja `std::filesystem::exists()` zwraca wartość typu `bool` - `true` jeśli dany katalog istnieje, `false` jeśli nie. Jeśli jednak chcemy uzyskać więcej informacji o danym katalogu, możemy wykorzystać funkcję `std::filesystem::directory_entry()`.

Ta funkcja tworzy obiekt `std::filesystem::directory_entry`, który przechowuje informacje o konkretnym katalogu. Umożliwia ona dostęp do różnych informacji, takich jak nazwa katalogu, rozmiar, data ostatniej modyfikacji itp.

## Zobacz także

- [Dokumentacja funkcji std::filesystem::exists()](https://en.cppreference.com/w/cpp/filesystem/exists)
- [Dokumentacja funkcji std::filesystem::directory_entry()](https://en.cppreference.com/w/cpp/filesystem/directory_entry)