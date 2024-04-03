---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:20.734561-07:00
description: "\u041F\u0435\u0440\u0435\u0432\u0456\u0440\u043A\u0430 \u043D\u0430\u044F\
  \u0432\u043D\u043E\u0441\u0442\u0456 \u0434\u0438\u0440\u0435\u043A\u0442\u043E\u0440\
  \u0456\u0457 \u0432 Java \u2014 \u0446\u0435 \u043E\u0441\u043D\u043E\u0432\u043D\
  \u0430 \u0437\u0430\u0432\u0434\u0430\u043D\u043D\u044F, \u044F\u043A\u0430 \u043F\
  \u0435\u0440\u0435\u0434\u0431\u0430\u0447\u0430\u0454 \u0432\u0435\u0440\u0438\u0444\
  \u0456\u043A\u0430\u0446\u0456\u044E \u0456\u0441\u043D\u0443\u0432\u0430\u043D\u043D\
  \u044F \u0434\u0438\u0440\u0435\u043A\u0442\u043E\u0440\u0456\u0457 \u0444\u0430\
  \u0439\u043B\u043E\u0432\u043E\u0457 \u0441\u0438\u0441\u0442\u0435\u043C\u0438\
  \ \u043F\u0435\u0440\u0435\u0434 \u0457\u0457 \u0447\u0438\u0442\u0430\u043D\u043D\
  \u044F\u043C, \u0437\u0430\u043F\u0438\u0441\u043E\u043C\u2026"
lastmod: '2024-03-13T22:44:49.103758-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u0435\u0440\u0435\u0432\u0456\u0440\u043A\u0430 \u043D\u0430\u044F\
  \u0432\u043D\u043E\u0441\u0442\u0456 \u0434\u0438\u0440\u0435\u043A\u0442\u043E\u0440\
  \u0456\u0457 \u0432 Java \u2014 \u0446\u0435 \u043E\u0441\u043D\u043E\u0432\u043D\
  \u0430 \u0437\u0430\u0432\u0434\u0430\u043D\u043D\u044F, \u044F\u043A\u0430 \u043F\
  \u0435\u0440\u0435\u0434\u0431\u0430\u0447\u0430\u0454 \u0432\u0435\u0440\u0438\u0444\
  \u0456\u043A\u0430\u0446\u0456\u044E \u0456\u0441\u043D\u0443\u0432\u0430\u043D\u043D\
  \u044F \u0434\u0438\u0440\u0435\u043A\u0442\u043E\u0440\u0456\u0457 \u0444\u0430\
  \u0439\u043B\u043E\u0432\u043E\u0457 \u0441\u0438\u0441\u0442\u0435\u043C\u0438\
  \ \u043F\u0435\u0440\u0435\u0434 \u0457\u0457 \u0447\u0438\u0442\u0430\u043D\u043D\
  \u044F\u043C, \u0437\u0430\u043F\u0438\u0441\u043E\u043C \u0430\u0431\u043E \u0432\
  \u0438\u043A\u043E\u043D\u0430\u043D\u043D\u044F\u043C \u0431\u0443\u0434\u044C\
  -\u044F\u043A\u0438\u0445 \u043E\u043F\u0435\u0440\u0430\u0446\u0456\u0439, \u0434\
  \u043B\u044F \u044F\u043A\u0438\u0445 \u043F\u043E\u0442\u0440\u0456\u0431\u043D\
  \u043E \u0457\u0457 \u0456\u0441\u043D\u0443\u0432\u0430\u043D\u043D\u044F."
title: "\u041F\u0435\u0440\u0435\u0432\u0456\u0440\u043A\u0430 \u043D\u0430\u044F\u0432\
  \u043D\u043E\u0441\u0442\u0456 \u0434\u0438\u0440\u0435\u043A\u0442\u043E\u0440\u0456\
  \u0457"
weight: 20
---

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
