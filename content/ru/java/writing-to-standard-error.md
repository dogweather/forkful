---
title:                "Запись в стандартный вывод ошибок"
aliases:
- ru/java/writing-to-standard-error.md
date:                  2024-01-29T00:22:22.361858-07:00
model:                 gpt-4-0125-preview
simple_title:         "Запись в стандартный вывод ошибок"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/java/writing-to-standard-error.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Запись в стандартный поток ошибок (stderr) - это способ вывода сообщений об ошибках и диагностики, отдельно от стандартного вывода (stdout). Программисты используют его, чтобы сигнализировать о том, что произошло что-то исключительное, что облегчает отладку и изоляцию проблем.

## Как это сделать:

Java делает запись в stderr простой с использованием `System.err`. Взглянем быстро:

```java
public class StderrExample {
    public static void main(String[] args) {
        System.err.println("Ошибка: Что-то пошло не так!");
    }
}
```

Запуск этого дает вам:

```
Ошибка: Что-то пошло не так!
```

Обратите внимание: хотя stdout обычно идет на консоль, stderr может быть перенаправлен в файл или другое место назначения, сохраняя сообщения об ошибках отдельно.

## Глубокое погружение

Исторически в системах, подобных Unix, stderr является файловым дескриптором 2, отличным от stdout (файловый дескриптор 1). Это позволяет производить различную обработку и перенаправление. Альтернативы `System.err` включают в себя фреймворки логирования, такие как Log4J или SLF4J, которые предлагают больше возможностей. В Java stderr реализован в классе `System` и является экземпляром `PrintStream`. Он не буферизуется, что означает мгновенный вывод.

## Смотрите также

- [Документация Oracle Java - System.err](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#err)
- [Wikipedia - Стандартные потоки](https://en.wikipedia.org/wiki/Standard_streams)
- [Учебник по логированию в Java](https://www.baeldung.com/java-logging-intro)
