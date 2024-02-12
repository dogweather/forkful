---
title:                "Створення тимчасового файлу"
aliases: - /uk/cpp/creating-a-temporary-file.md
date:                  2024-01-20T17:39:47.584960-07:00
model:                 gpt-4-1106-preview
simple_title:         "Створення тимчасового файлу"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Що & Навіщо?
Створювання тимчасового файлу — це метод, щоб зберегти дані на короткий термін. Програмісти використовують це для маніпуляцій з даними, які не потрібно зберігати довго, або при тестуванні, щоб уникнути змін в основних файлах.

## Як зробити:
```C++
#include <iostream>
#include <filesystem>
#include <fstream>

int main() {
    // Створення унікального тимчасового файлу в тимчасовій директорії
    std::filesystem::path temp = std::filesystem::temp_directory_path() /= "my_tempfile_XXXXXX";

    // Використання mkstemp для безпечного створення тимчасового файлу
    int fd = mkstemp(temp.data());
    if (fd == -1) {
        std::cerr << "Не можу створити тимчасовий файл!\n";
        return 1;
    }

    // Робота з файлом через файловий потік
    std::ofstream temp_file(temp);
    temp_file << "Привіт, це тимчасовий файл!" << std::endl;
    temp_file.close();

    // Вивід шляху до тимчасового файлу
    std::cout << "Тимчасовий файл створено: " << temp << std::endl;

    // Прибирання: закриття файлового дескриптора і видалення файлу
    close(fd);
    std::filesystem::remove(temp);

    return 0;
}
```

## Поглиблений Розділ:
Створення тимчасового файлу - стандартна задача в UNIX-подібних системах, яка історично вирішувалася функцією `tmpfile()` та макросом `tmpnam()`, але вони мають проблеми з безпекою через можливі змагання станів (race conditions). `mkstemp()` мінімізує цей ризик, автоматично замінюючи `XXXXXX` на унікальний набір символів та відкриваючи файл без можливості перехоплення. В C++, зі стандартом C++17, `std::filesystem` ідеально підходить для роботи з файловою системою, оскільки вона надає системно-незалежний інтерфейс. Тимчасові файли важливі для зберігання тимчасових даних без зайвого забруднення файлової системи і можуть бути видалені без сліду після використання. 

## Дивіться Також:
- [std::filesystem](https://en.cppreference.com/w/cpp/filesystem)
- [POSIX mkstemp](https://man7.org/linux/man-pages/man3/mkstemp.3.html)
- [C++ File I/O](https://www.cplusplus.com/doc/tutorial/files/)
