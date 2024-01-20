---
title:                "Перевірка наявності директорії"
html_title:           "Bash: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?

Перевірка наявності директорії - це процес, під час якого програма визначає, чи існує певний шлях до папки у файловій системі. Програмісти роблять це, щоб уникнути помилок при спробі доступу або зміні файлів у неіснуючій директорії.

## Як це зробити:

C++17 запроваджує бібліотеку `<filesystem>`, що значно спрощує роботу з файлами і директоріями. Ось приклад перевірки наявності директорії:

```C++
#include <iostream>
#include <filesystem>

int main() {
    std::filesystem::path dirPath = "/some/dir";

    if (std::filesystem::exists(dirPath)) {
        std::cout << "Directory exists: " << dirPath << '\n';
    } else {
        std::cout << "Directory does not exist: " << dirPath << '\n';
    }

    return 0;
}
```

Прикладний вивід:
```
Directory exists: "/some/dir"
```
або
```
Directory does not exist: "/some/dir"
```

## Детальний огляд:

В історичному контексті, до C++17, перевірка наявності директорії в C++ була більш складною і залежала від платформи (наприклад, використання `stat` в POSIX). `<filesystem>` спрощує процес, надаючи платформо-незалежний API.

Існують альтернативи: використання `boost::filesystem` для старих версій C++ або прямі системні виклики (`_access` в Windows, `stat` в Unix-подібних системах). 

Про деталі реалізації: `<filesystem>` використовує RAII (Resource Acquisition Is Initialization), гарантуючи коректне управління ресурсами. Клас `std::filesystem::path` дозволяє маніпулювати шляхами, а функції типу `exists()`, `is_directory()`, `create_directory()` полегшують роботу з файловою системою.

## Дивіться також:

- [Документація по std::filesystem](https://en.cppreference.com/w/cpp/filesystem)
- [Boost.Filesystem, якщо ви використовуєте старший стандарт C++](https://www.boost.org/doc/libs/1_75_0/libs/filesystem/doc/index.htm)
- [Tutorial for file I/O in C++](https://www.cplusplus.com/doc/tutorial/files/)