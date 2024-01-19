---
title:                "Перевірка наявності директорії"
html_title:           "C: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Що й навіщо?
Перевірка наявності директорії - це процесс, в якому програма визначає, чи існує конкретна директорія. Це необхідно, аби  уникнути помилок, що виникають, коли програма намагається виконувати операції з директорією, що не існує.

## Як це зробити:
Порожня директорія може бути перевірена використовуючи системний виклик `stat` або `access`.

```C
#include <sys/stat.h>
#include <errno.h>

int is_dir_exist(const char* path) {
    struct stat statbuf;
    if (stat(path, &statbuf) != 0) {
        return 0;
    }
    return S_ISDIR(statbuf.st_mode);
}
```
Програма поверне 0, якщо директорія не існує, і 1, якщо існує.

## Пірнемо глибше:
**Історичний контекст:** Функції `stat` та `access` були представлені в UNIX ще в 70-х. Вони є стандартними функціями в більшості POSIX-сумісних систем.

**Альтернативи:** Помімо `stat`, може бути використана функція `opendir`. Але вона вимагає більше системних ресурсів, адже відкриває директорію.

**Подробиці реалізації:** Функція `stat` заповнює структуру `struct stat`, яка містить інформацію про файл (включаючи директорії). `S_ISDIR` є макросом, який перевіряє, чи є файл директорією.

## Дивитись також:
* Опис структури `struct stat` у [документації Linux](http://man7.org/linux/man-pages/man2/stat.2.html)
* Як використовувати функцію `opendir` для [відкриття директорій](https://www.gnu.org/software/libc/manual/html_node/Opening-a-Directory.html) 
* Детальний огляд [перевірки наявності директорій у C](https://stackoverflow.com/questions/12510874/how-can-i-check-if-a-directory-exists) на StackOverflow.