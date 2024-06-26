---
date: 2024-01-20 17:54:37.291064-07:00
description: "\u042F\u043A? \u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0444\u0430\
  \u0439\u043B\u0456\u0432 \u0443 Java \u0432\u0456\u0434\u0431\u0443\u0432\u0430\u043B\
  \u043E\u0441\u044C \u0447\u0435\u0440\u0435\u0437 `FileInputStream` \u0430\u0431\
  \u043E `BufferedReader`. \u0417 \u043F\u043E\u044F\u0432\u043E\u044E NIO (New Input/Output)\
  \ \u0443 Java 7, \u0437'\u044F\u0432\u0438\u043B\u0438\u0441\u044C \u043A\u043B\u0430\
  \u0441\u0438 `Path` \u0456\u2026"
lastmod: '2024-04-05T21:53:49.317947-06:00'
model: gpt-4-1106-preview
summary: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0444\u0430\u0439\u043B\u0456\
  \u0432 \u0443 Java \u0432\u0456\u0434\u0431\u0443\u0432\u0430\u043B\u043E\u0441\u044C\
  \ \u0447\u0435\u0440\u0435\u0437 `FileInputStream` \u0430\u0431\u043E `BufferedReader`."
title: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
weight: 22
---

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
