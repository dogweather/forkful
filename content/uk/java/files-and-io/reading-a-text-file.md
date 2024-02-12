---
title:                "Читання текстового файлу"
date:                  2024-01-20T17:54:37.291064-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання текстового файлу"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Що і чому?
Читання текстових файлів — це процес забезпечення доступу до змісту файлів для маніпуляції чи вивчення. Програмісти читають файли, щоб отримати дані, налаштування, скрипти чи шаблони, як основу для подальших обчислень або як частину вводу/виводу програми.

## Як? 
```java
import java.nio.file.*;
import java.io.IOException;

public class FileReaderExample {
    public static void main(String[] args) {
        Path path = Paths.get("example.txt");
        try {
            String content = Files.readString(path);
            System.out.println("Файл містить наступний текст:");
            System.out.println(content);
        } catch (IOException e) {
            System.out.println("Помилка при читанні файлу: " + e.getMessage());
        }
    }
}
```
Вивід:
```
Файл містить наступний текст:
Це приклад тексту у файлі.
```

## Поглиблений розбір
Читання файлів у Java відбувалось через `FileInputStream` або `BufferedReader`. З появою NIO (New Input/Output) у Java 7, з'явились класи `Path` і `Files`, що спростили процедуру. Крім методу `readString`, існує метод `readAllLines`, який повертає список рядків і корисний для роботи з багаторядковими файлами.

Метод `Files.readString` з Java 11 зчитує увесь файл одним викликом, що є простіше і часто швидше, але може споживати більше пам'яті для великих файлів. Для величезних файлів краще використовувати `BufferedReader`, який читає файли частинами.

Важливими також є питання безпеки та управління ресурсами. Завжди слід перевіряти підозрілі шляхи до файлів і гарантувати, що потоки закриті, щоб уникнути витоку ресурсів.

## Також читайте
- [Docs.oracle.com – Path class](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Path.html)
- [Docs.oracle.com – Files class](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)
- [Baeldung.com – Reading a File into a String](https://www.baeldung.com/java-read-file)
- [Stackoverflow – When to use FileReader vs BufferedReader?](https://stackoverflow.com/questions/9648811/specific-difference-between-bufferedreader-and-filereader)