---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:58.196406-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: C++ \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442 `cerr`\
  \ \u0434\u043B\u044F \u0437\u0430\u043F\u0438\u0441\u0438 \u0432 `stderr`. \u0412\
  \u043E\u0442 \u043F\u0440\u0438\u043C\u0435\u0440 \u0438\u0441\u043F\u043E\u043B\
  \u044C\u0437\u043E\u0432\u0430\u043D\u0438\u044F."
lastmod: '2024-03-13T22:44:45.637309-06:00'
model: gpt-4-0125-preview
summary: "C++ \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442 `cerr`\
  \ \u0434\u043B\u044F \u0437\u0430\u043F\u0438\u0441\u0438 \u0432 `stderr`."
title: "\u0417\u0430\u043F\u0438\u0441\u044C \u0432 \u0441\u0442\u0430\u043D\u0434\
  \u0430\u0440\u0442\u043D\u044B\u0439 \u043F\u043E\u0442\u043E\u043A \u043E\u0448\
  \u0438\u0431\u043E\u043A"
weight: 25
---

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
