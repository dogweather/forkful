---
title:                "Перевірка наявності директорії"
html_title:           "C++: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Що і навіщо?

Перевірка на існування директорії - це процес, який визначає, чи наявна вказана директорія. Програмісти це роблять, щоб уникнути помилок при спробі відкриття або зміни директорій, які не існують.

## Як це робиться:

```C++
#include <filesystem>

bool is_directory_exists(const std::filesystem::path& p)
{
    return std::filesystem::exists(p) && std::filesystem::is_directory(p);
}

int main()
{
    std::filesystem::path p{"/path/to/directory"};
    if (is_directory_exists(p)) {
        std::cout << "Directory exists.\n";
    } else {
        std::cout << "Directory does not exist.\n";
    }
    return 0;
}
```

При відповідному шляху до директорії, вивід коду може бути таким:

```
Directory exists.
```

## Пірнемо глибше:

**Історичний контекст** - перевірка існування директорії в C++ не завжди була пряма. Перш ніж з’явилося стандартне бібліотеку <filesystem>, програмісти використовували бібліотеки сторонніх розробників або системні виклики.

**Альтернативи** - в деяких випадках, якщо ви працюєте з C++17 або старіше, можете використовувати `std::filesystem::is_directory()`, який також перевіряє існування перед визначенням, чи є шлях директорією.

**Деталі впровадження** - `std::filesystem::exists(p)` перевіряє, чи існує файл / директорія, тоді як `std::filesystem::is_directory(p)` перевіряє, чи є об'єкт директорією.

## Дивіться також:

- C++ документація std::filesystem::exists: https://en.cppreference.com/w/cpp/filesystem/exists
- C++ документація std::filesystem::is_directory: https://en.cppreference.com/w/cpp/filesystem/is_directory
- Навчальний посібник з файлової системи C++17 від Microsoft: https://docs.microsoft.com/en-us/cpp/standard-library/file-system-navigation?view=msvc-160