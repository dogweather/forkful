---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:55:40.389106-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412\u043E\u0442 \u043A\u0430\u043A \u043F\u0440\u043E\u0432\u0435\
  \u0440\u0438\u0442\u044C \u0441\u0443\u0449\u0435\u0441\u0442\u0432\u0443\u0435\u0442\
  \ \u043B\u0438 \u043A\u0430\u0442\u0430\u043B\u043E\u0433 \u0441 `java.nio.file`."
lastmod: '2024-03-13T22:44:44.849037-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u043E\u0442 \u043A\u0430\u043A \u043F\u0440\u043E\u0432\u0435\u0440\
  \u0438\u0442\u044C \u0441\u0443\u0449\u0435\u0441\u0442\u0432\u0443\u0435\u0442\
  \ \u043B\u0438 \u043A\u0430\u0442\u0430\u043B\u043E\u0433 \u0441 `java.nio.file`."
title: "\u041F\u0440\u043E\u0432\u0435\u0440\u043A\u0430 \u0441\u0443\u0449\u0435\u0441\
  \u0442\u0432\u043E\u0432\u0430\u043D\u0438\u044F \u0434\u0438\u0440\u0435\u043A\u0442\
  \u043E\u0440\u0438\u0438"
weight: 20
---

## Как это сделать:
Вот как проверить существует ли каталог с `java.nio.file`:

```java
import java.nio.file.Files;
import java.nio.file.Path;

public class DirectoryCheck {

    public static void main(String[] args) {
        Path directoryPath = Path.of("/path/to/directory");

        // Проверяем, существует ли каталог
        boolean directoryExists = Files.exists(directoryPath);

        // Выводим результат
        System.out.println("Существует ли каталог? " + directoryExists);
    }
}
```

Если вы запустите это, в вашей консоли просто отобразится:

```
Существует ли каталог? true // или false
```

Ударяйте.

## Погружение в детали
В прошлом люди использовали метод `java.io.File.exists()`. Но теперь актуальность приобрел `java.nio.file.Files.exists(Path)`, поскольку он более универсален. С помощью того же API вы также можете проверять атрибуты файла.

Но это еще не все. Метод `Files.exists` не является непробиваемым — есть условия гонки. Что, если с каталогом что-то случится сразу после проверки? Бац, и операция проваливается. Чтобы этого избежать, используйте `Files.exists` с осторожностью и корректно обрабатывайте исключения при выполнении операций с файлами.

В качестве альтернативы, вы можете просто попытаться выполнить операцию с файлом и поймать возможное исключение `NoSuchFileException`. Это известно как подход "легче просить прощения, чем разрешения" (EAFP) против "смотри прежде, чем прыгнуть" (LBYL), который использует `Files.exists()`.

## Смотрите также
- [Files.exists()](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Files.html#exists(java.nio.file.Path,java.nio.file.LinkOption...))
- [Ввод и вывод файлов в Java](https://docs.oracle.com/javase/tutorial/essential/io/)
- Классная статья о EAFP против LBYL: [Принцип EAFP](https://devblogs.microsoft.com/python/idiomatic-python-eafp-versus-lbyl/)
