---
title:    "Java: Перевірка наявності каталогу"
keywords: ["Java"]
---

{{< edit_this_page >}}

##Чому

Перевірка існування директорії є важливою складовою будь-якої програми, особливо в тому випадку, коли ми працюємо з файлами та даними на комп'ютері. Знання, як перевірити, чи існує певна директорія, допоможе нам побудувати більш робочі та ефективні програми.

##Як це зробити

```Java
import java.io.File;

public class DirectoryChecker {
    public static void main(String[] args) {
        //вказуємо шлях до директорії, яку хочемо перевірити
        File dir = new File("C:/Users/user/Desktop");
        
        //використовуємо метод exists(), щоб перевірити, чи існує директорія
        if (dir.exists()) { 
            System.out.println("Директорія існує!");
        } else {
            System.out.println("Директорія не існує!");
        }
    }
}
```

Вихідний результат програми:

```
Директорія існує!
```

Для більш специфічної перевірки, також можна використовувати методи isDirectory() та isFile(). Перший метод перевіряє, чи є даний об'єкт директорією, а другий - чи є файлом. Давайте подивимося на приклад:

```Java
import java.io.File;

public class DirectoryChecker {
    public static void main(String[] args) {
        File file = new File("C:/Users/user/Desktop/file.txt");
        
        if (file.isDirectory()) {
            System.out.println("Це директорія.");
        } else if (file.isFile()) {
            System.out.println("Це файл.");
        } else {
            System.out.println("Данний об'єкт не є ні файлом, ні директорією.");
        }
    }
}
```

Вихідний результат програми:

```
Це файл.
```

##Детальний розбір

Існує декілька методів, які можна використовувати для перевірки існування директорії. Один з них - exists(), використаний в наших прикладах. Цей метод повертає значення true, якщо директорія існує, або false, якщо її не існує. Також, для проведення операцій над вмістом директорії, можна використовувати методи list() або listFiles().

Будьте пильні при використанні методу exists(). Він також поверне значення true, якщо переданий шлях не є директорією, але може бути файлом з таким ім'ям.

##Дивіться також

- [Java File Class](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- [Java File Handling](https://www.geeksforgeeks.org/file-handling-java/)
- [Java IO Tutorial](https://www.tutorialspoint.com/java/io/index.htm)