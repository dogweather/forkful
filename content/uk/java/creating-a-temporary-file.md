---
title:                "Створення тимчасового файлу"
html_title:           "Java: Створення тимчасового файлу"
simple_title:         "Створення тимчасового файлу"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Чому
Створення тимчасового файлу є корисною технікою у програмуванні, оскільки допомагає зберегти та обробляти дані, які не потрібні на постійній основі.

## Як
```Java
import java.io.File;
import java.io.IOException;

public class TemporaryFileExample {
    public static void main(String[] args) throws IOException {
        // Створення об'єкту для тимчасового файлу у папці по замовчуванню
        File tempFile = File.createTempFile("example", ".txt");

        // Запис деяких даних у тимчасовий файл
        // Якщо файл не існує, ця дія створить його
        FileWriter writer = new FileWriter(tempFile);
        writer.write("Це приклад тимчасового файлу!");
        writer.close();

        // Виведення шляху до створеного тимчасового файлу
        System.out.println("Шлях до тимчасового файлу: " + tempFile.getAbsolutePath());

        // Видалення тимчасового файлу після завершення програми
        tempFile.deleteOnExit();
    }
}
```

Вихідний код показує, як створити тимчасовий файл і записати дані до нього, а також як видалити його після виконання програми. Також можна вказати бажане розширення файлу, вказуючи другий аргумент у методі `createTempFile()`. Детальніше про роботу з тимчасовими файлами можна дізнатися у документації Java.

## Глибше
Коли ми створюємо тимчасовий файл, він зберігається у системній папці для тимчасових файлів, яка залежить від операційної системи. Як правило, це `AppData` для Windows, `TMP` для MacOS та `/tmp` для Unix-подібних систем. Якщо необхідно, можна вказати іншу папку для збереження тимчасових файлів, використовуючи спеціальну системну властивість `java.io.tmpdir`.

Ще одним корисним варіантом використання тимчасових файлів є їх використання у тестуванні програм. Вони дозволяють створювати і тестувати файли без засмічення основної файлової системи, оскільки вони будуть автоматично видалені у кінці тестів.

## Дивіться також
- [Документація Java про тимчасові файли](https://docs.oracle.com/javase/10/docs/api/java/io/File.html#createTempFile(java.lang.String,java.lang.String,java.io.File))
- [Робота з файлами у Java](https://www.javatpoint.com/java-file)