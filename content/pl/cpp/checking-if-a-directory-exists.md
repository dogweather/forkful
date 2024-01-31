---
title:                "Sprawdzanie, czy katalog istnieje"
date:                  2024-01-19
simple_title:         "Sprawdzanie, czy katalog istnieje"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Po co sprawdzać, czy katalog istnieje? Niektóre operacje wymagają pewności, że miejsce docelowe jest dostępne przed zapisaniem plików czy danych. Sprawdzanie obecności katalogu unika błędów i pomaga w zarządzaniu plikami.

## How to:
```C++
#include <filesystem>
#include <iostream>

int main() {
    std::filesystem::path dir_to_check{"./some_directory"};

    if (std::filesystem::exists(dir_to_check)) {
        std::cout << "Directory exists." << std::endl;
    } else {
        std::cout << "Directory does not exist." << std::endl;
    }

    return 0;
}
```
Output, depending on your directory:
```
Directory exists.
```
or
```
Directory does not exist.
```

## Deep Dive
Stosujemy `<filesystem>`, nowość od C++17, który uprościł pracę z systemem plików. Historycznie, programiści używali różnych metod, jak `stat` w POSIX czy `GetFileAttributes` w WinAPI, które były mniej przenośne i wymagały więcej kodu. Opcje są różne: `boost::filesystem` dla starszych projektów, czy systemowe wywołania dla lepszego dostosowania. W implementacji, `exists()` korzysta z wywołań systemowych, więc jej wydajność i zachowanie mogą zależeć od systemu operacyjnego.

## See Also
- Dokumentacja `std::filesystem::exists`: https://en.cppreference.com/w/cpp/filesystem/exists
- Moduł `<filesystem>`: https://en.cppreference.com/w/cpp/header/filesystem
- Porównanie `boost::filesystem` i `std::filesystem`: https://www.boost.org/doc/libs/1_75_0/libs/filesystem/doc/index.htm
