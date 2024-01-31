---
title:                "Створення текстового файлу"
date:                  2024-01-19
simple_title:         "Створення текстового файлу"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Що та чому?
Запис текстового файлу – це процес зберігання текстових даних на диск за допомогою програмування. Програмісти роблять це для постійного зберігання даних, обміну інформацією або логування подій системи.

## Як це зробити:
```Java
import java.io.FileWriter;
import java.io.IOException;

public class TextFileWriter {
    public static void main(String[] args) {
        String data = "Вітаю, це тестовий текст!";
        try {
            FileWriter writer = new FileWriter("example.txt");
            writer.write(data);
            writer.close();
            System.out.println("Файл успішно записано!");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

## Поглиблений огляд
У минулому програмісти використовували `FileOutputStream`, але з Java 1.1 з’явився `FileWriter`, що спростив запис текстових файлів. Як альтернатива, `Files` API (з Java 7 NIO) пропонує метод `Files.write()`, який дозволяє зробити це одним рядком коду. Важливо розуміти кодування файлу (наприклад, UTF-8) та правильне закриття файла після запису через `try-with-resources`.

## Дивіться також
- [JavaDoc FileWriter](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/io/FileWriter.html)
- [Oracle Tutorial on I/O](https://docs.oracle.com/javase/tutorial/essential/io/)
- [Baeldung Guide to Java FileWriter](https://www.baeldung.com/java-write-to-file)
