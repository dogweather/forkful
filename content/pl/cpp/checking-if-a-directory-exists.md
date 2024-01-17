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

## Co & Dlaczego?
Sprawdzanie, czy katalog istnieje, jest procesem, którego używają programiści do upewnienia się, czy dany katalog faktycznie istnieje. Jest to ważne, ponieważ programy często muszą operować na istniejących katalogach lub tworzyć nowe, w zależności od potrzeb. Wiele funkcji w języku C++ wymaga, aby użytkownik podał poprawną ścieżkę do katalogu, więc upewnienie się, że dany katalog istnieje jest niezbędne.

## Jak to zrobić:
Sprawdzenie, czy katalog istnieje, jest stosunkowo proste w języku C++. W poniższym przykładzie użyjemy funkcji stat z biblioteki <sys/stat.h> aby sprawdzić, czy katalog istnieje. Funkcja ta zwraca wartość 0, jeśli katalog istnieje, lub wartość -1, jeśli nie istnieje.

```C++
#include <sys/stat.h>
#include <iostream>

int main() {
    struct stat buffer;
    if (stat("katalog_testowy", &buffer) == 0)
        std::cout << "Katalog istnieje" << std::endl;
    else
        std::cout << "Katalog nie istnieje" << std::endl;
    return 0;
}
```
Sample output:
```
Katalog istnieje
```

## Głębokie Nurkowanie:
Funkcja stat, użyta w powyższym przykładzie, jest dostępna w języku C++ od jego wczesnych wersji. Alternatywnym podejściem jest użycie boost::filesystem, które zapewnia bogatszy zestaw funkcji do operacji na katalogach i plikach. Aby sprawdzić, czy katalog istnieje przy użyciu boost::filesystem możemy użyć funkcji exists.

```C++
#include <boost/filesystem.hpp>
#include <iostream>

int main() {
    if (boost::filesystem::exists("katalog_testowy"))
        std::cout << "Katalog istnieje" << std::endl;
    else
        std::cout << "Katalog nie istnieje" << std::endl;
    return 0;
}
```

W języku C++17 pojawiła się również funkcja filesystem::exists, która zwraca typ std::filesystem::file_status zamiast bool. Aby zobaczyć pełną listę dostępnych funkcji dotyczących operacji na katalogach i plikach, można przejrzeć dokumentację języka C++ lub boost::filesystem.

## Zobacz też:
- [Dokumentacja języka C++](https://en.cppreference.com/w/)
- [Dokumentacja boost::filesystem](https://www.boost.org/doc/libs/1_76_0/libs/filesystem/doc/index.htm)