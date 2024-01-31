---
title:                "Читання текстового файлу"
date:                  2024-01-20T17:54:05.308008-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання текстового файлу"

category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Читання текстового файлу — це процес, при якому зчитуються дані з файлу на диску у вашу програму. Робимо це, аби обробити, відобразити, чи зберегти дані.

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
