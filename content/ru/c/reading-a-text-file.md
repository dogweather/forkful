---
title:                "Чтение текстового файла"
date:                  2024-01-29T00:01:44.360977-07:00
model:                 gpt-4-0125-preview
simple_title:         "Чтение текстового файла"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c/reading-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

Чтение текстового файла — это доступ к данным файла в виде строкового содержимого, посимвольно или построчно. Программисты делают это для обработки, анализа или манипулирования хранимой информацией без ручного ввода при каждом выполнении.

## Как:

Давайте прочитаем текстовый файл. Мы откроем его, прочитаем и закроем. Основные вещи.

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *file;
    char filename[] = "example.txt";
    char ch;

    file = fopen(filename, "r"); // Открытие файла в режиме чтения

    if (file == NULL) {
        perror("Ошибка при открытии файла.\n");
        exit(EXIT_FAILURE);
    }

    printf("Содержимое %s:\n", filename);

    while ((ch = fgetc(file)) != EOF) { // Чтение и печать символ за символом
        putchar(ch);
    }

    fclose(file); // Закрытие файла

    return 0;
}
```

Предполагая, что в `example.txt` находится "Hello, C!", вывод будет:
```
Содержимое example.txt:
Hello, C!
```

## Погружение

Ещё в 70-х годах родился язык C, и с ним — тот способ чтения файлов, который мы используем сегодня. Это не ракетостроение, но есть свои нюансы. Вы используете `fopen` для открытия файлов и `fgetc` для чтения по одному символу за раз. Но почему по символу за символом? Вы могли бы читать строки с помощью `fgets` или целиком файл с помощью `fread`, если это подходит для вашего случая. Всё дело в контроле и в том, что вашей программе нужно обработать.

За кулисами `fopen` говорит операционной системе: "Эй, мне нужен этот файл, дай мне доступ!" И система говорит ок, возвращая указатель `FILE`. Функция `fgetc` шепчет указателю файла: "Дай мне следующий байт, ладно?" И он делает это, пока не дойдет до EOF, маркера конца файла.

Альтернативы? Конечно. У вас есть `fscanf` для форматированного чтения, `getline` для современных ребят или системные вызовы `read` низкого уровня, если вы хотите быть ближе к "железу". И не забудьте, после последнего прочитанного байта, быть вежливым и выполнить `fclose` файла.

## Смотрите также

Для более глубокого изучения ознакомьтесь с:

- Документацией стандартной библиотеки C: [https://en.cppreference.com/w/c/io](https://en.cppreference.com/w/c/io)
- Руководством по библиотеке GNU C: [https://www.gnu.org/software/libc/manual/html_node/I_002fO-Overview.html](https://www.gnu.org/software/libc/manual/html_node/I_002fO-Overview.html)
- Узнайте больше о различных функциях чтения: [https://www.tutorialspoint.com/c_standard_library/c_function_fread.htm](https://www.tutorialspoint.com/c_standard_library/c_function_fread.htm)
- Для по-настоящему любопытных, глубокое погружение в системные вызовы Linux: [https://man7.org/linux/man-pages/man2/read.2.html](https://man7.org/linux/man-pages/man2/read.2.html)
