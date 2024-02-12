---
title:                "Перевірка наявності директорії"
aliases: - /uk/cpp/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:14.261970-07:00
model:                 gpt-4-0125-preview
simple_title:         "Перевірка наявності директорії"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?
Перевірка наявності директорії полягає у визначенні присутності директорії за вказаним шляхом перед виконанням операцій, як-от читання з файлів або запис у них. Програмісти роблять це, щоб уникнути помилок, пов’язаних із операціями з файлами, забезпечуючи більш плавне та надійне виконання завдань із обробки файлів у своїх програмах.

## Як це зробити:
У сучасному C++ (C++17 і новіші версії), можна використовувати бібліотеку filesystem для перевірки наявності директорії. Вона забезпечує простий і стандартизований спосіб виконання операцій із файловою системою, включаючи перевірку наявності директорії.

```cpp
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main() {
    const fs::path dirPath = "/path/to/directory";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "Директорія існує." << std::endl;
    } else {
        std::cout << "Директорія не існує." << std::endl;
    }

    return 0;
}
```
Приклад виводу, якщо директорія існує:
```
Директорія існує.
```

Приклад виводу, якщо директорія не існує:
```
Директорія не існує.
```

Для проектів, які ще не використовують C++17, або для додаткових можливостей, бібліотека Boost Filesystem є популярним вибором сторонніх розробників, яка пропонує схожу функціональність.

```cpp
#include <iostream>
#include <boost/filesystem.hpp>

namespace fs = boost::filesystem;

int main() {
    const fs::path dirPath = "/path/to/directory";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "Директорія існує." << std::endl;
    } else {
        std::cout << "Директорія не існує." << std::endl;
    }

    return 0;
}
```
Використовуючи Boost Filesystem, вивід буде ідентичний прикладу з файловою системою C++17, залежно від наявності директорії за вказаним шляхом.
