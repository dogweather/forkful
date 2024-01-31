---
title:                "Проверка существования директории"
date:                  2024-01-28T23:55:40.389106-07:00
model:                 gpt-4-0125-preview
simple_title:         "Проверка существования директории"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/java/checking-if-a-directory-exists.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и зачем?
Проверка существования каталога означает верификацию его наличия перед тем, как попытаться чтение или запись файлов в нем. Программисты делают это, чтобы избежать ошибок, например, попытки сохранения файла там, где нет места для его размещения.

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
