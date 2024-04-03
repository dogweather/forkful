---
date: 2024-01-20 17:54:05.308008-07:00
description: "How to: (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438:) ."
lastmod: '2024-03-13T22:44:49.876413-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
weight: 22
---

## How to: (Як це зробити:)
```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream inputFile("example.txt");
    std::string line;
    
    if (inputFile.is_open()) {
        while (getline(inputFile, line)) {
            std::cout << line << '\n';
        }
        inputFile.close();
    } else {
        std::cerr << "Unable to open file";
    }

    return 0;
}
```
** Sample Output (Приклад Виведення): **
```
Hello, World!
This is a text file reading example.
```

## Deep Dive (Поглиблений Аналіз):
Читання файлів у C++ було завжди важливою частиною багатьох програм. Використання класів `ifstream` для вводу та `ofstream` для виводу від часів C++98 стандарту дало нам стандартизований спосіб роботи з I/O. На вибір є й інші методи, як наприклад `FILE*` з C, але вони менш безпечні і зручні. Роблячи читання файлу, важливо правильно обробити помилки, як за допомогою перевірок, так і через винятки.

## See Also (Дивіться також):
- [std::ifstream](http://www.cplusplus.com/reference/fstream/ifstream/)
- [File I/O in C++](https://en.cppreference.com/w/cpp/io/basic_fstream)
- [C++ Programming Language](https://isocpp.org/)
- [Working with files in C++](https://www.learncpp.com/cpp-tutorial/186-basic-file-io/)
