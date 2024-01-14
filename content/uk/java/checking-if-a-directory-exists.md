---
title:                "Java: Перевірка наявності директорії"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Чому

Перевірка існування директорії є важливим етапом в роботі багатьох програм, особливо тих, що працюють з файловою системою. Цей процес дозволяє зберегти час та зусилля і уникнути помилок при взаємодії з файлами та директоріями.

## Як

```Java
import java.io.File;
public class DirectoryChecker {
    public static void main(String[] args) {
        String path = "C://Users//Username//Documents";
        File directory = new File(path);
        
        if (directory.exists()) {
           System.out.println("Директорія існує.");
        } else {
           System.out.println("Директорія не існує.");
        }
    }
}
```

В даному прикладі ми використовуємо клас `File` для створення об'єкту директорії. Метод `exists()` перевіряє, чи існує директорія за вказаним шляхом. Далі за допомогою умовного оператору `if` ми виводимо відповідне повідомлення на екран.

Ви також можете використовувати метод `isDirectory()` для перевірки, чи вказаний об'єкт є директорією, а не просто файлом.

```Java
if (directory.isDirectory()) {
   System.out.println("Це директорія.");
} else {
   System.out.println("Це файл.");
}
```

## Глибоке поглиблення

Перевірка існування директорії може бути корисною не тільки при створенні файлів або папок, але і при роботі з існуючими. Наприклад, ви можете перевірити, чи існують всі необхідні директорії перед збереженням файлів в певне місце.

Також, якщо директорія не існує, ви можете створити її за допомогою методу `mkdir()` або `mkdirs()`, який автоматично створює всі необхідні батьківські директорії, якщо вони не існують. 

## Дивись також

- [Клас File у Java](https://www.javatpoint.com/java-file)
- [Робота з файлами та директоріями у Java](https://metanit.com/java/tutorial/10.6.php)
- [Робота зі строками у Java](https://prog.kiev.ua/forum/topic/2056-rabota-so-strokami-vo-vremya-programmirovaniya-na-java/)