---
title:                "Java: Написання текстового файлу"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Чому

Написання текстового файлу є важливою частиною програмування, оскільки це дозволяє зберігати та передавати дані для подальшої обробки. Також текстові файли є універсальним форматом, який може бути використаний на різних операційних системах та програмах.

## Як

Для написання текстового файлу в Java потрібно використати клас `FileWriter`. Давайте розглянемо приклад коду, який заносить дані у текстовий файл і виводить його в консоль:

```java
import java.io.FileWriter;
import java.io.IOException;

public class TextFileWriter {

    public static void main(String[] args) {

        try {
            FileWriter writer = new FileWriter("example.txt"); // створюємо об'єкт FileWriter з назвою файлу
            writer.write("Це приклад тексту, який буде записаний у файл."); // записуємо текст у файл
            writer.close(); // закриваємо потік
            System.out.println("Текст успішно записаний у файл.");
        } catch (IOException e) {
            System.out.println("Сталася помилка при записі у файл.");
            e.printStackTrace();
        }
    }
}
```

Ви можете запустити цей код та перевірити, що він створив текстовий файл з назвою `example.txt` та заніс у нього вміст "Це приклад тексту, який буде записаний у файл.". Також цей текст буде виведений у консоль.

## Глибше

Написання текстового файлу також може включати роботу з іншими класами та методами, які дозволяють форматувати, додавати заголовки та інші елементи до файлу. Також важливо пам'ятати про закривання потоків після завершення роботи з файлом, адже це дозволяє звільнити пам'ять та уникнути можливих проблем.

## Дивіться також

- [Документація Java з написання текстових файлів](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
- [Докладний огляд FileWriter класу](https://www.tutorialspoint.com/java/io/java_io_filewriter.htm)
- [Детальна стаття про роботу з текстовими файлами в Java](https://www.geeksforgeeks.org/reading-writing-text-files-java/)