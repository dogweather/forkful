---
date: 2024-01-20 17:40:58.468857-07:00
description: "\u0421\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0442\u0438\u043C\
  \u0447\u0430\u0441\u043E\u0432\u0438\u0445 \u0444\u0430\u0439\u043B\u0456\u0432\
  \ \u0443 Java - \u0446\u0435 \u043F\u0440\u043E \u0442\u0435, \u044F\u043A \u0433\
  \u0435\u043D\u0435\u0440\u0443\u0432\u0430\u0442\u0438 \u0444\u0430\u0439\u043B\u0438\
  \ \u0434\u043B\u044F \u0442\u0438\u043C\u0447\u0430\u0441\u043E\u0432\u043E\u0433\
  \u043E \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F\
  . \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\
  \u0431\u043B\u044F\u0442\u044C \u0446\u0435, \u043A\u043E\u043B\u0438 \u0457\u043C\
  \ \u043F\u043E\u0442\u0440\u0456\u0431\u043D\u043E \u0431\u0435\u0437\u043F\u0435\
  \u0447\u043D\u043E \u0456\u2026"
lastmod: '2024-02-25T18:49:46.595231-07:00'
model: gpt-4-1106-preview
summary: "\u0421\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0442\u0438\u043C\
  \u0447\u0430\u0441\u043E\u0432\u0438\u0445 \u0444\u0430\u0439\u043B\u0456\u0432\
  \ \u0443 Java - \u0446\u0435 \u043F\u0440\u043E \u0442\u0435, \u044F\u043A \u0433\
  \u0435\u043D\u0435\u0440\u0443\u0432\u0430\u0442\u0438 \u0444\u0430\u0439\u043B\u0438\
  \ \u0434\u043B\u044F \u0442\u0438\u043C\u0447\u0430\u0441\u043E\u0432\u043E\u0433\
  \u043E \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F\
  . \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\
  \u0431\u043B\u044F\u0442\u044C \u0446\u0435, \u043A\u043E\u043B\u0438 \u0457\u043C\
  \ \u043F\u043E\u0442\u0440\u0456\u0431\u043D\u043E \u0431\u0435\u0437\u043F\u0435\
  \u0447\u043D\u043E \u0456\u2026"
title: "\u0421\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0442\u0438\u043C\u0447\
  \u0430\u0441\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
---

{{< edit_this_page >}}

## What & Why? | Що і Чому?
Створення тимчасових файлів у Java - це про те, як генерувати файли для тимчасового використання. Програмісти роблять це, коли їм потрібно безпечно і швидко обробити дані, не захаращуючи систему постійними файлами.

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
