---
title:                "Извлечение подстрок"
date:                  2024-01-28T23:57:44.340852-07:00
model:                 gpt-4-0125-preview
simple_title:         "Извлечение подстрок"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c/extracting-substrings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Извлечение подстрок означает захват специфической части строки - кусочка пирога. Программисты делают это для того, чтобы изолировать, обработать или манипулировать только релевантными битами данных в большем тексте.

## Как это сделать:
Давайте извлечем некоторые подстроки из строки на языке C.

```C
#include <stdio.h>
#include <string.h>

void extract_substring(const char *source, int start, int length, char *dest) {
    strncpy(dest, source + start, length);
    dest[length] = '\0'; // Не забудьте о завершающем нуле!
}

int main() {
    const char *full_text = "Extracting substrings is neat.";
    char snippet[20];

    extract_substring(full_text, 0, 10, snippet);
    printf("Фрагмент: %s\n", snippet);

    extract_substring(full_text, 12, 10, snippet);
    printf("Еще один: %s\n", snippet);

    return 0;
}
```

Пример вывода:

```
Фрагмент: Extracting
Еще один: substrings
```

## Глубже в тему
Извлечение подстрок... это вовсе не новинка. В мире программирования на C это обычная задача с момента появления языка в 1970-х годах.

У вас есть альтернативные способы захвата этих подстрок. Некоторые люди используют `strncpy()`, как в нашем примере выше. Другие могут предпочесть `sscanf()` или даже вручную перебирать строку. У каждого подхода есть свои нюансы. Будьте осторожны с `strncpy()` - если вы укажете длину, превышающую конец строки, она не добавит завершающий ноль.

Под капотом строка в C — это просто массив символов. Во время извлечения подстроки, вы работаете с указателями на конкретные адреса в памяти. Следите за границами и всегда добавляйте завершающий ноль в ваши фрагменты.

## Смотрите также
- Руководство по `strncpy()`: https://www.man7.org/linux/man-pages/man3/strncpy.3.html
- Обработка строк в C: https://en.cppreference.com/w/c/string
- Указатели и массивы: https://www.tutorialspoint.com/cprogramming/c_pointers.htm
