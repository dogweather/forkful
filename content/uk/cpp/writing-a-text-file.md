---
title:                "Написання текстового файлу"
aliases:
- uk/cpp/writing-a-text-file.md
date:                  2024-02-03T19:27:43.458127-07:00
model:                 gpt-4-0125-preview
simple_title:         "Написання текстового файлу"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?
Запис у текстовий файл в C++ передбачає створення або відкриття файлу, а потім запис даних до нього, що є фундаментальним завданням для додатків, яким потрібно зберігати дані, таких як логи, зміст, створений користувачами, або налаштування конфігурації. Програмісти роблять це для збереження даних, генерованих під час виконання програми, або для експорту даних для використання іншими програмами або користувачами.

## Як це зробити:
C++ пропонує кілька способів запису в текстовий файл, але один з найпростіших методів - використання бібліотеки `<fstream>`, яка надає клас `ofstream` (вихідний файловий потік), призначений для операцій запису у файл.

### Приклад використання `<fstream>`:

```cpp
#include <fstream>
#include <iostream>

int main() {
    std::ofstream file("example.txt");
    if (file.is_open()) {
        file << "Привіт, світ!\n";
        file << "Запис у файл в C++ простий.";
        file.close();
    } else {
        std::cerr << "Не вдалося відкрити файл\n";
    }
    return 0;
}
```

**Приклад виводу в 'example.txt':**
```
Привіт, світ!
Запис у файл в C++ простий.
```

При роботі з більш складними даними або коли потрібен більший контроль над процесом запису, програмісти можуть звернутися до сторонніх бібліотек, таких як Boost Filesystem.

### Приклад використання Boost Filesystem:

Щоб використовувати Boost для операцій з файлами, спершу потрібно встановити бібліотеки Boost. Наступний приклад демонструє створення та запис у файл за допомогою `boost::filesystem` і `boost::iostreams`.

```cpp
#include <boost/filesystem.hpp>
#include <boost/iostreams/device/file.hpp>
#include <boost/iostreams/stream.hpp>
#include <iostream>

namespace io = boost::iostreams;
namespace fs = boost::filesystem;

int main() {
    fs::path filePath("boost_example.txt");
    io::stream_buffer<io::file_sink> buf(filePath.string());
    std::ostream out(&buf);
    out << "Boost робить операції з файлами простими.\n";
    out << "Це рядок, написаний за допомогою Boost.";
    
    return 0;
}
```

**Приклад виводу в 'boost_example.txt':**
```
Boost робить операції з файлами простими.
Це рядок, написаний за допомогою Boost.
```

Вибір між сирим C++ та сторонньою бібліотекою, як-от Boost, може залежати від конкретних вимог вашого проекту та того, скільки контролю або гнучкості вам потрібно над операціями вводу/виводу файлів.
