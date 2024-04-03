---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:01:34.346501-07:00
description: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430 \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0435\u0442, \u0447\u0442\u043E \u0432\u0430\u0448\u0430 \u043F\u0440\
  \u043E\u0433\u0440\u0430\u043C\u043C\u0430 \u0437\u0430\u0433\u0440\u0443\u0436\u0430\
  \u0435\u0442 \u0441\u043E\u0434\u0435\u0440\u0436\u0438\u043C\u043E\u0435 \u0444\
  \u0430\u0439\u043B\u0430 \u0432 \u0432\u0438\u0434\u0435 \u0441\u0442\u0440\u043E\
  \u043A\u0438. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\
  \u044B \u0434\u0435\u043B\u0430\u044E\u0442 \u044D\u0442\u043E \u0434\u043B\u044F\
  \ \u043E\u0431\u0440\u0430\u0431\u043E\u0442\u043A\u0438 \u0438\u043B\u0438 \u0430\
  \u043D\u0430\u043B\u0438\u0437\u0430 \u0434\u0430\u043D\u043D\u044B\u0445,\u2026"
lastmod: '2024-03-13T22:44:44.855142-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430 \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0435\u0442, \u0447\u0442\u043E \u0432\u0430\u0448\u0430 \u043F\u0440\
  \u043E\u0433\u0440\u0430\u043C\u043C\u0430 \u0437\u0430\u0433\u0440\u0443\u0436\u0430\
  \u0435\u0442 \u0441\u043E\u0434\u0435\u0440\u0436\u0438\u043C\u043E\u0435 \u0444\
  \u0430\u0439\u043B\u0430 \u0432 \u0432\u0438\u0434\u0435 \u0441\u0442\u0440\u043E\
  \u043A\u0438."
title: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\u043E\u0432\
  \u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430"
weight: 22
---

## Как:
Чтение файла в Java - это проще простого, особенно с использованием `java.nio.file`. Вот быстрый пример:

```java
import java.nio.file.Files;
import java.nio.file.Path;
import java.io.IOException;
import java.util.stream.Stream;

public class FileReadExample {
    public static void main(String[] args) {
        Path filePath = Path.of("example.txt");

        try (Stream<String> lines = Files.lines(filePath)) {
            lines.forEach(System.out::println);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Запуск этого кода с `example.txt`, содержащим "Hello, file readers!", выведет:

```
Hello, file readers!
```

## Погружаемся глубже
Java развивается. Раньше вам приходилось самостоятельно управлять потоками и читателями – множество шаблонного кода. Пакет `java.io` был на пике популярности, с часто используемыми `FileReader` и `BufferedReader`. Затем появился `java.nio`, предлагающий каналы и буферы для большего контроля.

Теперь `java.nio.file` находится еще на более высоком уровне. `Files` и `Paths` упрощают задачу. Приведенный выше пример использует `Files.lines`, которое лениво стримит строки, что отлично подходит для больших файлов. Также вы получаете try-with-resources, автоматически закрывающее потоки, чтобы избежать утечек.

Альтернативы? `Scanner` удобен для анализа. Apache Commons IO и Guava от Google предлагают утилиты для более сложных задач, если они вам нужны. Тем не менее, обычная Java обычно вас далеко заведет.

С точки зрения реализации, кодировка файла имеет значение. `Files.lines` по умолчанию использует UTF-8, но вы можете указать другую. С другой стороны, `BufferedReader` требует, чтобы вы заранее установили `Charset`, если он не является стандартным.

## Смотрите также
Для дополнительной информации загляните сюда:

- Класс [`Files`](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Files.html) в официальной документации Java.
- [Чтение, запись и создание файлов](https://docs.oracle.com/javase/tutorial/essential/io/file.html) для подробного ознакомления.
- [Apache Commons IO](https://commons.apache.org/proper/commons-io/) для мощной библиотеки утилит ввода-вывода файлов.
