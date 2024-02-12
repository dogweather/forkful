---
title:                "Чтение текстового файла"
aliases:
- /ru/java/reading-a-text-file/
date:                  2024-01-29T00:01:34.346501-07:00
model:                 gpt-4-0125-preview
simple_title:         "Чтение текстового файла"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/java/reading-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?

Чтение текстового файла означает, что ваша программа загружает содержимое файла в виде строки. Программисты делают это для обработки или анализа данных, находящихся в файлах на их диске. Это основа для задач, таких как конфигурация, анализ данных или даже просто для извлечения вашего списка дел.

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
