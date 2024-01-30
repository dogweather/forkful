---
title:                "Создание временного файла"
date:                  2024-01-28T23:56:49.949880-07:00
model:                 gpt-4-0125-preview
simple_title:         "Создание временного файла"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/java/creating-a-temporary-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Создание временного файла означает создание файла, который нужен только на короткое время, а затем удаляется. Программисты делают это для промежуточного хранения, например, когда вам нужно временно сохранить данные между этапами процесса или держать конфиденциальную информацию вне долгосрочного хранения.

## Как это сделать:

В Java пакет `java.nio.file` - ваш друг для работы с временными файлами. Посмотрите на этот фрагмент:

```java
import java.nio.file.Files;
import java.nio.file.Path;

public class TempFileExample {
    public static void main(String[] args) {
        try {
            // Создание временного файла
            Path tempFile = Files.createTempFile(null, ".tmp");
            System.out.println("Временный файл создан в: " + tempFile);

            // Запись во временный файл
            Files.writeString(tempFile, "Это содержимое временного файла");

            // Чтение из временного файла
            String content = Files.readString(tempFile);
            System.out.println("Содержимое временного файла: " + content);

            // Удаление временного файла (опционально, так как он удаляется при выходе из JVM)
            Files.delete(tempFile);
            System.out.println("Временный файл удален.");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Запустите его, и вы получите что-то вроде:

```
Временный файл создан в: /tmp/user23423842348234823948.tmp
Содержимое временного файла: Это содержимое временного файла
Временный файл удален.
```

Неплохо, правда?

## Подробнее

Временные файлы были частью нашего инструментария веками, начиная с зари вычислительной техники. Они ваш лучший выбор, когда вам нужно обработать данные, которые не должны оставаться.

Java поддерживает вас с классом `Files` начиная с Java 7, делая обработку временных файлов очень простой. До этого приходилось манипулировать объектами `File` и надеяться на лучшее (но не возвращайтесь к тем темным дням, обнимите новое API).

Крутая часть метода `createTempFile` заключается в том, что вы можете указать директорию и префикс или суффикс имени файла, или оставить это на усмотрение Java по умолчанию. Просто помните, если вы не удалите эти файлы вручную, они останутся до выхода из программы. И в некоторых случаях, особенно с долгосрочными приложениями, вы захотите сделать очистку самостоятельно, вместо того чтобы ждать финала.

Альтернативы? Конечно, вы могли бы пойти старым путем и обрабатывать каждую операцию с файлами вручную или использовать специфический для ОС метод. Однако, путь Java безопаснее и более переносим между платформами.

## Смотрите также

- [Документация по классу Java Path](https://docs.oracle.com/javase/10/docs/api/java/nio/file/Path.html)
- [Документация по классу Java Files](https://docs.oracle.com/javase/10/docs/api/java/nio/file/Files.html)
- [Учебное пособие Oracle по файловому вводу/выводу](https://docs.oracle.com/javase/tutorial/essential/io/file.html)