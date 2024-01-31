---
title:                "Перевірка наявності директорії"
date:                  2024-01-19
simple_title:         "Перевірка наявності директорії"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Перевірка існування каталогу - це процес визначення, чи фактично є каталог на файловій системі. Програмісти роблять це, щоб уникнути помилок під час читання, запису або створення файлів.

## How to: (Як це зробити:)
```C
#include <stdio.h>
#include <sys/stat.h>

int main() {
    struct stat statbuf;
    char *dirPath = "/path/to/directory";

    // Перевіряємо каталог на існування
    if (stat(dirPath, &statbuf) == 0 && S_ISDIR(statbuf.st_mode)) {
        printf("Каталог існує.\n");
    } else {
        printf("Каталог не існує.\n");
    }
    
    return 0;
}
```
Приклад виведення:
```
Каталог існує.
```
або
```
Каталог не існує.
```

## Deep Dive (Поглиблений аналіз):
У ранніх версіях Unix, структури даних файлової системи були простішими, і перевірка існування каталогу вимагала більше зусиль. Зараз, функція `stat()` дає детальну інформацію про файл, включаючи тип (чи це каталог, символьне посилання тощо). Якщо `stat()` повертає 0, файл або каталог існує. Значення `S_ISDIR` оцінює, чи запис, до якого веде шлях, є каталогом.

Як альтернативу, можна використовувати `opendir()` із `<dirent.h>`, але це ефективніше для читання вмісту каталогів.

Увага, користувачі: права доступу до файлової системи можуть вплинути на результати перевірки існування каталогу. Без належних прав, `stat()` може повідомити, що каталог не існує, навіть якщо він цілком доступний з правами суперкористувача.

## See Also (Дивіться також):
- POSIX `stat` reference: https://pubs.opengroup.org/onlinepubs/9699919799/functions/stat.html
- GNU C Library - File Attributes: https://www.gnu.org/software/libc/manual/html_node/File-Attributes.html
- Stack Overflow discussion on checking directory existence: https://stackoverflow.com/questions/3828192/checking-if-a-directory-exists-in-unix-system-call
