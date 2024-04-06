---
date: 2024-01-20 17:40:58.468857-07:00
description: "How to: | \u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438: Sample Output."
lastmod: '2024-04-05T22:38:48.184587-06:00'
model: gpt-4-1106-preview
summary: "| \u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438:\
  \ Sample Output."
title: "\u0421\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0442\u0438\u043C\u0447\
  \u0430\u0441\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
weight: 21
---

## How to: | Як це зробити:
```java
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

public class TempFileExample {
    public static void main(String[] args) {
        try {
            // Створюємо тимчасовий файл
            File tempFile = Files.createTempFile("example", ".tmp").toFile();
            System.out.println("Тимчасовий файл створено: " + tempFile.getAbsolutePath());

            // Робимо щось із файлом...

            // Видалення тимчасового файлу при виході з програми
            tempFile.deleteOnExit();

        } catch (IOException e) {
            System.out.println("Помилка при створенні тимчасового файлу.");
            e.printStackTrace();
        }
    }
}
```
Sample Output:
```
Тимчасовий файл створено: C:\Users\...\AppData\Local\Temp\example1234567890.tmp
```

## Deep Dive | Глибоке Занурення:
Система тимчасових файлів в Java базується на підході, який використовувався ще в UNIX-системах. Ключова ідея - це безпека та мінімізація ризику конфліктів імен файлів чи витоку даних.

Альтернативи створенню тимчасових файлів можуть включати використання вбудованої пам'яті (RAM) або спеціалізованих потокових обробників, але вони можуть бути менш надійними щодо збереження даних під час падіння системи.

Коли викликається `Files.createTempFile()`, Java генерує унікальне ім'я файла і забезпечує його створення в тимчасовій директорії, яку вказує системна властивість `java.io.tmpdir`. Такий файл може бути видаленим вручну методом `delete()`, або він автоматично зникне при закінченні роботи програми, якщо використовується `deleteOnExit()`.

## See Also | Дивіться Також:
- [Java File I/O (NIO.2) - Oracle Docs](https://docs.oracle.com/javase/tutorial/essential/io/fileio.html)
- [Java 8 Tutorial - File API - Javapoint](https://www.javatpoint.com/java-nio)
- [Working with Temporary Files/Folders in Java - StackOverflow](https://stackoverflow.com/questions/16691437/how-to-create-temporary-file-in-java)
