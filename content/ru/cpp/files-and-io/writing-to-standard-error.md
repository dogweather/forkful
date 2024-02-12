---
title:                "Запись в стандартный поток ошибок"
aliases:
- /ru/cpp/writing-to-standard-error/
date:                  2024-01-29T00:05:58.196406-07:00
model:                 gpt-4-0125-preview
simple_title:         "Запись в стандартный поток ошибок"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/cpp/writing-to-standard-error.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Запись в стандартный поток ошибок (`stderr`) означает отправку сообщений об ошибках и диагностической информации в специальный поток, отдельный от обычного вывода (`stdout`). Программисты делают это для четкого разделения обычного вывода от ошибок, что делает вывод программы более удобным для обработки и отладки.

## Как это сделать:
C++ использует `cerr` для записи в `stderr`. Вот пример использования:

```cpp
#include <iostream>

int main() {
    std::cout << "Это обычный вывод" << std::endl;
    std::cerr << "Это сообщение об ошибке" << std::endl;
    return 0;
}
```

Пример вывода может выглядеть так:

```
Это обычный вывод
Это сообщение об ошибке
```

Даже если вы перенаправите `stdout`, `stderr` все равно будет отображаться в терминале:

```cpp
// Перенаправляем stdout в файл, но stderr все равно отображается в терминале
int main() {
    freopen("output.txt", "w", stdout);
    std::cout << "Это не будет показано в терминале" << std::endl;
    std::cerr << "Это будет показано в терминале" << std::endl;
    fclose(stdout);
    return 0;
}
```

## Глубокое погружение:
В системах, подобных UNIX, `stderr` был введен для разделения вывода программы (`stdout`) от сообщений об ошибках, причем каждый из них имеет свой файловый дескриптор (1 для `stdout`, 2 для `stderr`). Альтернативы `cerr` - это использование `fprintf(stderr, ...)` на языке C или непосредственная запись в файловый дескриптор 2. Внутренне, `cerr` является экземпляром `ostream` и не буферизуется, чтобы обеспечить немедленный вывод ошибок без ожидания заполнения буфера.

## Смотрите также:
- [cppreference std::cerr](https://en.cppreference.com/w/cpp/io/cerr)
- [GNU C Library: Стандартные потоки](https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html)
- [Перенаправление stdout и stderr](http://www.cplusplus.com/reference/cstdio/freopen/)
