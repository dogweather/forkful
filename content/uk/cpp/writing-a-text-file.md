---
title:                "Створення текстового файлу"
date:                  2024-01-19
html_title:           "Arduino: Створення текстового файлу"
simple_title:         "Створення текстового файлу"

category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Що та Чому?

Запис текстових файлів – це процес запису даних у файл з розширенням, зазвичай `.txt`. Програмісти роблять це для збереження даних, що можуть бути з легкістю зчитані й оброблені як людиною, так і програмою.

## Як це зробити:

```C++
#include <fstream>
#include <iostream>
#include <string>

int main() {
    std::string text = "Привіт, світ!";
    std::ofstream outFile("приклад.txt");

    if (outFile.is_open()) {
        outFile << text;
        outFile.close();
        std::cout << "Файл успішно записаний!" << std::endl;
    } else {
        std::cerr << "Помилка при відкритті файла!" << std::endl;
    }

    return 0;
}
```

Sample output:
```
Файл успішно записаний!
```

## Поглиблений Розгляд

В історичному контексті, запис файлів був одним з основних способів тривалої збереження даних. Сучасні альтернативи включають бази даних та хмарні сховища. Щодо імплементації, C++ використовує класи fstream для файлової взаємодії. При написанні файлу важливо обробляти помилки, наприклад, використовуючи is_open() для перевірки відкриття файлу.

## Дивись Також

- Документація по `fstream`: http://www.cplusplus.com/reference/fstream/
- Бібліотека Boost.Filesystem для більш функціональної роботи з файлами: https://www.boost.org/doc/libs/release/libs/filesystem/
- Туторіал по роботі з файлами у C++: http://www.cplusplus.com/doc/tutorial/files/
