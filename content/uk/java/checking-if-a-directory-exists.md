---
title:                "Перевірка наявності директорії"
date:                  2024-01-20T14:57:36.218214-07:00
simple_title:         "Перевірка наявності директорії"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Що це та Навіщо?
Перевірка існування каталогу - це спосіб з'ясувати, чи є у файловій системі певна папка. Програмісти роблять це, щоб уникнути помилок при роботі з файлами або для створення каталогів, якщо таких ще немає.

## Як це зробити:
```java
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class DirectoryExists {
    public static void main(String[] args) {
        // Перевірка чи існує каталог
        Path path = Paths.get("/path/to/your/directory");

        if (Files.exists(path)) {
            System.out.println("Каталог існує!");
        } else {
            System.out.println("Каталог не існує!");
        }
    }
}
```
Вивід залежатиме від того, чи існує каталог на вашому комп'ютері:
```
Каталог існує!
```
або
```
Каталог не існує!
```

## Поглибленно:
Перевірка існування каталогу у Java використовує класи з пакету `java.nio.file` введеного у Java 7. Це більш гнучкий та комплексний спосіб роботи з файловою системою порівняно зі старішим `java.io.File`. `Path` та `Files` дозволяють легше моделювати шляхи у файловій системі, і забезпечують кращу переносимість коду між різними ОС. Альтернативою є використання методу `File.exists()`, але в разі роботи зі складнішими файловими операціями `java.nio.file` надає переваги іншим інструментам.

## Дивіться також:
- [Official Java Documentation for Files](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Files.html)
- [Oracle Tutorial on File I/O](https://docs.oracle.com/javase/tutorial/essential/io/)
- [Stack Overflow discussion on checking a directory's existence](https://stackoverflow.com/questions/3634853/how-to-create-a-directory-in-java)
