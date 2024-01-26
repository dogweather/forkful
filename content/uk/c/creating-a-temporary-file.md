---
title:                "Створення тимчасового файлу"
date:                  2024-01-20T17:39:54.127652-07:00
model:                 gpt-4-1106-preview
simple_title:         "Створення тимчасового файлу"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why? (Що таке та навіщо?)
Створення тимчасового файлу - це процес, коли ваша програма створює файл, що існуватиме тільки під час її роботи. Програмісти роблять це для безпеки даних, уникнення конфліктів і зменшення нагрузки на оперативну пам'ять.

## How to (Як це зробити):
```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    char temp_filename[] = "tmpfile_XXXXXX";
    int file_descriptor = mkstemp(temp_filename);
    
    if (file_descriptor == -1) {
        perror("Error creating temporary file");
        return EXIT_FAILURE;
    }

    write(file_descriptor, "Hello, temporary world!", 23);

    // Remember to close the file descriptor
    close(file_descriptor);

    // Optionally delete the file if it's no longer needed
    unlink(temp_filename);

    printf("Temporary file '%s' created.\n", temp_filename);

    return EXIT_SUCCESS;
}
```
Sample Output:
```
Temporary file 'tmpfile_a1b2c3' created.
```

## Deep Dive (Поглиблений огляд):
Процес створення тимчасових файлів бере свій початок із ранніх днів UNIX систем, де було критично важливо зберігати надійність і атомарність операцій. Функція `mkstemp` створює тимчасовий файл з унікальним ім'ям, що мінімізує ризик перезапису файлів. Існують альтернативи, як `tmpfile()`, яка створює анонімний тимчасовий файл, що автоматично вилучається при закритті. Проте `mkstemp` дає більше контролю, наприклад, підтримує встановлення прав доступу до файлу. Важливо вчасно закривати тимчасові файли та видаляти їх, коли вони більше не потрібні, щоб уникнути витоку ресурсів.

## See Also (Дивіться також):
- [The Open Group Base Specifications Issue 7, 2018 edition - mkstemp(3)](https://pubs.opengroup.org/onlinepubs/9699919799/functions/mkstemp.html)
- [GNU C Library Reference Manual - Temporary Files](https://www.gnu.org/software/libc/manual/html_node/Temporary-Files.html)
- [stackoverflow - What is the difference between mkstemp() and tmpfile()?](https://stackoverflow.com/questions/41542960/what-is-the-difference-between-mkstemp-and-tmpfile)
