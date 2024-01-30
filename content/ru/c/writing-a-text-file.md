---
title:                "Создание текстового файла"
date:                  2024-01-29T00:05:23.766618-07:00
model:                 gpt-4-0125-preview
simple_title:         "Создание текстового файла"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c/writing-a-text-file.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Создание текстового файла включает сохранение данных в читаемом формате на файловой системе. Программисты делают это для сохранения информации, такой как настройки, логи или пользовательский контент, для последующего извлечения и обработки.

## Как это сделать:
Запись в текстовый файл на C выполняется просто с помощью функций библиотеки `stdio.h`: `fopen()`, `fprintf()` и `fclose()`. Посмотрите на этот простой пример:

```C
#include <stdio.h>

int main() {
    FILE *filePointer = fopen("example.txt", "w"); // Открытие файла в режиме записи
    if (filePointer == NULL) {
        printf("Ошибка открытия файла.\n");
        return 1;
    }
    
    fprintf(filePointer, "Привет, мир!\n"); // Запись в файл
    fprintf(filePointer, "Запись в файлы на C проста.\n");
    
    fclose(filePointer); // Закрытие файла
    return 0;
}
```
Пример вывода в `example.txt`:
```
Привет, мир!
Запись в файлы на C проста.
```

## Подробнее
С момента появления языков-предшественников C, ввод-вывод файлов всегда был критически важен для программ. Альтернативы `stdio.h` включают системные вызовы, такие как `open()`, `write()` и `close()` из `sys/file.h`, которые предлагают больший контроль, но являются более сложными. При использовании `stdio.h` буферизация может влиять на производительность, поэтому для больших файлов или частой записи может потребоваться функция `fflush()`.

## Смотрите также
Для дополнительной информации о работе с файлами на C:
- Документация стандартной библиотеки C: https://en.cppreference.com/w/c/io
- Учебник по вводу-выводу файлов на C: http://www.cplusplus.com/doc/tutorial/files/
- Управление вводом-выводом файлов: https://www.gnu.org/software/libc/manual/html_node/File-System-Interface.html