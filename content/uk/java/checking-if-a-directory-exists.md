---
title:    "Java: Перевірка наявності каталогу"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Для чого

Перевірка існування директорії є важливим кроком у роботі з файлами і структурами каталогів у Java. Вона дозволяє перевірити, чи наявна потрібна нам директорія перед намаганням працювати з ними. Це уникне помилок та допоможе поліпшити продуктивність роботи з файловою системою.

# Як це зробити

Для того, щоб перевірити існування директорії в Java, ми можемо скористатися методом `exists()` класу `File`. Для цього необхідно створити об’єкт типу `File` з шляхом до директорії, яку ми хочемо перевірити. Приклад коду можна побачити нижче.

```Java
import java.io.File;

public class DirectoryExistence {

    public static void main(String[] args) {

        // шлях до директорії, яку ми хочемо перевірити
        String directoryPath = "C:\\Users\\User\\Documents\\MyDirectory";
        
        // створення об’єкту типу File
        File directory = new File(directoryPath);

        // перевірка існування директорії
        if (directory.exists()) {
            System.out.println("Директорія існує.");
        } else {
            System.out.println("Директорія не існує.");
        }
    }

}
```

Після запуску цього коду, якщо директорія `MyDirectory` існує, ми побачимо повідомлення "Директорія існує." у консолі. Якщо директорія не існує - "Директорія не існує.".

# Поглиблене дослідження

Існує кілька інших способів перевірки існування директорії в Java. Для прикладу, ми можемо використовувати метод `isDirectory()` для перевірки, чи переданий об'єкт є директорією. Також, можна використовувати метод `listFiles()`, який повертає масив файлів у директорії, або метод `list()`, який повертає масив імен файлів у директорії. Крім того, бібліотека Apache Commons IO містить метод `DirectoryUtils.exists()` для перевірки існування директорії, який є зручнішим для використання.

# Дивіться також

- [Офіційна документація Java: `File.exists()`](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/io/File.html#exists())
- [Офіційна документація Java: `File.isDirectory()`](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/io/File.html#isDirectory())
- [Офіційна документація Java: `File.listFiles()`](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/io/File.html#listFiles())
- [Офіційна документація Java: `File.list()`](https://docs.oracle.com/en/java