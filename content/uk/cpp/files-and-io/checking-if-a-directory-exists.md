---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:14.261970-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0423 \u0441\u0443\u0447\u0430\u0441\u043D\u043E\u043C\u0443 C++ (C++17 \u0456\
  \ \u043D\u043E\u0432\u0456\u0448\u0456 \u0432\u0435\u0440\u0441\u0456\u0457), \u043C\
  \u043E\u0436\u043D\u0430 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\
  \u0443\u0432\u0430\u0442\u0438 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\
  \u0443 filesystem \u0434\u043B\u044F \u043F\u0435\u0440\u0435\u0432\u0456\u0440\u043A\
  \u0438 \u043D\u0430\u044F\u0432\u043D\u043E\u0441\u0442\u0456 \u0434\u0438\u0440\
  \u0435\u043A\u0442\u043E\u0440\u0456\u0457. \u0412\u043E\u043D\u0430 \u0437\u0430\
  \u0431\u0435\u0437\u043F\u0435\u0447\u0443\u0454\u2026"
lastmod: '2024-03-13T22:44:49.870692-06:00'
model: gpt-4-0125-preview
summary: "\u0423 \u0441\u0443\u0447\u0430\u0441\u043D\u043E\u043C\u0443 C++ (C++17\
  \ \u0456 \u043D\u043E\u0432\u0456\u0448\u0456 \u0432\u0435\u0440\u0441\u0456\u0457\
  ), \u043C\u043E\u0436\u043D\u0430 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\
  \u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\
  \u0435\u043A\u0443 filesystem \u0434\u043B\u044F \u043F\u0435\u0440\u0435\u0432\u0456\
  \u0440\u043A\u0438 \u043D\u0430\u044F\u0432\u043D\u043E\u0441\u0442\u0456 \u0434\
  \u0438\u0440\u0435\u043A\u0442\u043E\u0440\u0456\u0457."
title: "\u041F\u0435\u0440\u0435\u0432\u0456\u0440\u043A\u0430 \u043D\u0430\u044F\u0432\
  \u043D\u043E\u0441\u0442\u0456 \u0434\u0438\u0440\u0435\u043A\u0442\u043E\u0440\u0456\
  \u0457"
weight: 20
---

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
