---
title:                "Читання текстового файлу"
html_title:           "Arduino: Читання текстового файлу"
simple_title:         "Читання текстового файлу"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Що це таке і навіщо?

Читання текстового файлу - це процес збору інформації, що зберігається у текстовому форматі з файлу. Програмісти це роблять, щоб маніпулювати даними та використовувати їх в своїх програмах.

## Як це зробити:

У Java є кілька способів роботи з текстовими файлами. Ось два основні приклади:

```Java
// Метод за допомогою Files
import java.nio.file.*;

public class ReadFile {
  public static void main(String[] args) {
    Path file = Paths.get("file.txt");
    try {
      byte[] data = Files.readAllBytes(file);
      System.out.println(new String(data));
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
```

```Java
// Метод за допомогою BufferedReader
import java.io.*;

public class ReadFile {
  public static void main(String[] args) {
    try {
      BufferedReader reader = new BufferedReader(new FileReader("file.txt"));
      String line;
      while((line = reader.readLine()) != null) {
        System.out.println(line);
      }
      reader.close();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
```

## Поглиблено:

Для читання текстових файлів, Java пропонує ряд інших методів, включаючи Scanner або FileInputStream. Ці класи були розроблені в різний час, як вдосконалення і адаптації до нових вимог та стандартів. Вибір методу залежить від конкретних потреб та вимог вашого проекту.

## Дивіться також:

- [Документація Java SE](https://docs.oracle.com/javase/tutorial/), щоб дізнатися більше про роботу з файлами та стрічками в Java.
- [Stack Overflow](https://stackoverflow.com/questions/4716503/reading-a-plain-text-file-in-java), тут ви можете знайти корисні обговорення про читання файлів в Java.