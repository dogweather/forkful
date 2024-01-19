---
title:                "Перевірка наявності директорії"
html_title:           "Java: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Що & Чому?
Перевірка наявності директорії в Java - це процес визначення того, чи існує даний каталог чи ні. Програмісти це роблять, щоб завчасно виявити можливі проблеми чи помилки, пов'язані з відсутністю очікуваного каталогу.

## Як це зробити:
Ми можемо використати стандартний метод `Files.exists()` з пакету `java.nio.file` для перевірки існування директорії.

```Java
import java.nio.file.*;

public class Main {
    public static void main(String[] args) {
        Path path = Paths.get("/path/to/directory");
        
        if (Files.exists(path)) {
            System.out.println("Каталог існує.");
        } else {
            System.out.println("Каталог не існує.");
        }
    }
}
```
Вихідний код:
```
Каталог існує.
```
або
```
Каталог не існує.
```

## Поглиблений аналіз
Перевірка наявності директорії може здаватися простою операцією, але історично вона була більш складною через різниці в операційних системах. З `java.nio.file`, введеним у Java 7, це значно спростилося. Щодо альтернатив, ви також можете використовувати `File.exists()` з `java.io.File`, але це менш надійний і менш гнучкий варіант.

## Дивіться також
- [Oracle documentation for java.nio.file.Files](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)
- [StackOverflow: How to check if a folder exists](https://stackoverflow.com/questions/3775694/how-to-check-if-a-folder-exists)