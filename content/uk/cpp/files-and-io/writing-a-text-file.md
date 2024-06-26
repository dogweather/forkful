---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:43.458127-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : C++ \u043F\u0440\u043E\u043F\u043E\u043D\u0443\u0454 \u043A\u0456\u043B\u044C\u043A\
  \u0430 \u0441\u043F\u043E\u0441\u043E\u0431\u0456\u0432 \u0437\u0430\u043F\u0438\
  \u0441\u0443 \u0432 \u0442\u0435\u043A\u0441\u0442\u043E\u0432\u0438\u0439 \u0444\
  \u0430\u0439\u043B, \u0430\u043B\u0435 \u043E\u0434\u0438\u043D \u0437 \u043D\u0430\
  \u0439\u043F\u0440\u043E\u0441\u0442\u0456\u0448\u0438\u0445 \u043C\u0435\u0442\u043E\
  \u0434\u0456\u0432 - \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\
  \u043D\u044F \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438 `<fstream>`,\
  \ \u044F\u043A\u0430 \u043D\u0430\u0434\u0430\u0454 \u043A\u043B\u0430\u0441\u2026"
lastmod: '2024-03-13T22:44:49.878682-06:00'
model: gpt-4-0125-preview
summary: "C++ \u043F\u0440\u043E\u043F\u043E\u043D\u0443\u0454 \u043A\u0456\u043B\u044C\
  \u043A\u0430 \u0441\u043F\u043E\u0441\u043E\u0431\u0456\u0432 \u0437\u0430\u043F\
  \u0438\u0441\u0443 \u0432 \u0442\u0435\u043A\u0441\u0442\u043E\u0432\u0438\u0439\
  \ \u0444\u0430\u0439\u043B, \u0430\u043B\u0435 \u043E\u0434\u0438\u043D \u0437 \u043D\
  \u0430\u0439\u043F\u0440\u043E\u0441\u0442\u0456\u0448\u0438\u0445 \u043C\u0435\u0442\
  \u043E\u0434\u0456\u0432 - \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\
  \u043D\u043D\u044F \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438\
  \ `<fstream>`, \u044F\u043A\u0430 \u043D\u0430\u0434\u0430\u0454 \u043A\u043B\u0430\
  \u0441 `ofstream` (\u0432\u0438\u0445\u0456\u0434\u043D\u0438\u0439 \u0444\u0430\
  \u0439\u043B\u043E\u0432\u0438\u0439 \u043F\u043E\u0442\u0456\u043A), \u043F\u0440\
  \u0438\u0437\u043D\u0430\u0447\u0435\u043D\u0438\u0439 \u0434\u043B\u044F \u043E\
  \u043F\u0435\u0440\u0430\u0446\u0456\u0439 \u0437\u0430\u043F\u0438\u0441\u0443\
  \ \u0443 \u0444\u0430\u0439\u043B."
title: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\
  \u0442\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
weight: 24
---

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
