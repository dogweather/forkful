---
title:                "Перевірка наявності папки"
html_title:           "Java: Перевірка наявності папки"
simple_title:         "Перевірка наявності папки"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Чому

Перевірка існування директорії є важливою задачею для багатьох програм на Java. Вона дозволяє переконатися, що необхідна директорія доступна для використання та уникнути непередбачуваних помилок в програмі.

## Як це зробити

Перевірка існування директорії може бути здійснена за допомогою методу `exists()` класу `File`. Для цього потрібно спочатку створити об'єкт `File` і передати йому шлях до директорії. Потім можна використовувати метод `exists()` для перевірки наявності директорії і отримання булевого значення `true` або `false`.

```Java
File directory = new File("C:\\Users\\user\\Desktop\\my_directory");

if(directory.exists()){
    System.out.println("Директорія існує!");
} else {
    System.out.println("Директорія не існує!");
}
```

Ви також можете використовувати метод `isDirectory()` для перевірки, чи є даний об'єкт `File` директорією. Цей метод повертає булеве значення `true` або `false` в залежності від того, чи є даний об'єкт директорією.

```Java
File directory = new File("C:\\Users\\user\\Desktop\\my_directory");

if(directory.isDirectory()){
    System.out.println("Це директорія!");
} else {
    System.out.println("Це не директорія!");
}
```

## Глибший розгляд

Перевірка існування директорії є важливим етапом в роботі із файловою системою на Java. Це дозволяє відділити роботу з файлами, які існують, від тих, які є відсутніми, та зробити програму більш стійкою до помилок.

Крім методів `exists()` та `isDirectory()`, клас `File` також має інші корисні методи для роботи з директоріями, наприклад `mkdir()` та `mkdirs()`, які дозволяють створити нову директорію або навіть деяку ієрархічну структуру директорій.

## Дивіться також

- [Інформація про клас File у документації Java](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html)
- [Більше інформації про роботу з файлами на Java у статті "Working with Files in Java"](http://tutorials.jenkov.com/java-io/files.html)