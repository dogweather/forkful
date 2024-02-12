---
title:                "Перевірка наявності директорії"
aliases:
- /uk/java/checking-if-a-directory-exists.md
date:                  2024-02-03T19:08:20.734561-07:00
model:                 gpt-4-0125-preview
simple_title:         "Перевірка наявності директорії"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Перевірка наявності директорії в Java — це основна завдання, яка передбачає верифікацію існування директорії файлової системи перед її читанням, записом або виконанням будь-яких операцій, для яких потрібно її існування. Це важливо для того, аби уникнути помилок чи винятків у програмах, які взаємодіють із файловою системою, забезпечуючи плавніше виконання та кращий досвід користувача.

## Як:
В Java існує кілька способів перевірити існування директорії, головним чином за допомогою класів `java.nio.file.Files` і `java.io.File`.

**Використання `java.nio.file.Files`**:

Цей метод є рекомендованим у новіших версіях Java.

```java
import java.nio.file.Files;
import java.nio.file.Paths;

public class DirectoryExists {
    public static void main(String[] args) {
        // Вкажіть шлях до директорії тут
        String directoryPath = "path/to/directory";

        // Перевірка існування директорії
        if (Files.exists(Paths.get(directoryPath))) {
            System.out.println("Директорія існує.");
        } else {
            System.out.println("Директорії не існує.");
        }
    }
}
```
**Приклад виводу**:
```
Директорія існує.
```
Або
```
Директорії не існує.
```

**Використання `java.io.File`**:

Хоча `java.nio.file.Files` є рекомендованим варіантом, старший клас `java.io.File` також можна використовувати.

```java
import java.io.File;

public class DirectoryExistsLegacy {
    public static void main(String[] args) {
        // Вкажіть шлях до директорії тут
        String directoryPath = "path/to/directory";

        // Створення об'єкта File
        File directory = new File(directoryPath);

        // Перевірка існування директорії
        if (directory.exists() && directory.isDirectory()) {
            System.out.println("Директорія існує.");
        } else {
            System.out.println("Директорії не існує.");
        }
    }
}
```
**Приклад виводу**:
```
Директорія існує.
```
Або
```
Директорії не існує.
```

**Використання сторонніх бібліотек**:

Хоча стандартна бібліотека Java зазвичай достатня для цього завдання, сторонні бібліотеки, як-от Apache Commons IO, пропонують додаткові утиліти для роботи з файлами, які можуть бути корисними в складніших додатках.

**Apache Commons IO**:

Спершу додайте залежність Apache Commons IO до вашого проєкту. Потім ви можете використовувати її можливості для перевірки існування директорії.

```java
// Припускаємо, що Apache Commons IO додано до проєкту

import org.apache.commons.io.FileUtils;

public class DirectoryExistsCommons {
    public static void main(String[] args) {
        // Вкажіть шлях до директорії тут
        String directoryPath = "path/to/directory";

        // Використання FileUtils для перевірки
        boolean directoryExists = FileUtils.directoryContains(new File(directoryPath), null);

        if (directoryExists) {
            System.out.println("Директорія існує.");
        } else {
            System.out.println("Директорії не існує.");
        }
    }
}
```

**Примітка**: `FileUtils.directoryContains` перевіряє, чи директорія містить конкретний файл, але передаючи `null` як другий аргумент, ви можете використовувати його для перевірки існування директорії. Будьте обережні, оскільки це може бути не найпряміший або передбачений метод використання.

**Приклад виводу**:
```
Директорія існує.
```
Або
```
Директорії не існує.
```
