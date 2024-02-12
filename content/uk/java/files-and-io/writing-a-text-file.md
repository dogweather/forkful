---
title:                "Написання текстового файлу"
aliases:
- /uk/java/writing-a-text-file/
date:                  2024-02-03T19:28:30.455269-07:00
model:                 gpt-4-0125-preview
simple_title:         "Написання текстового файлу"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

Запис текстового файлу в Java полягає у використанні можливостей мови для створення та запису вмісту у файли на файловій системі. Програмісти роблять це з різних причин, таких як логування, експорт даних або збереження стану програми для подальшого отримання.

## Як зробити:

### Використовуючи `java.nio.file` (Стандартна Бібліотека)

Пакет New I/O (NIO) в Java (`java.nio.file`) надає більш універсальний підхід до роботи з файлами. Ось спрощений спосіб запису у файл використовуючи `Files.write()`:

```java
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

public class TextFileWriterNIO {
    public static void main(String[] args) {
        List<String> lines = Arrays.asList("Рядок 1", "Рядок 2", "Рядок 3");
        try {
            Files.write(Paths.get("example.txt"), lines);
            System.out.println("Файл успішно записано!");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Вивід:

```
Файл успішно записано!
```

### Використовуючи `java.io` (Стандартна Бібліотека)

Для більш традиційного підходу, `java.io.FileWriter` - хороший вибір для простого запису текстових файлів:

```java
import java.io.FileWriter;
import java.io.IOException;

public class TextFileWriterIO {
    public static void main(String[] args) {
        try (FileWriter writer = new FileWriter("example.txt")) {
            writer.write("Привіт, Світ!\n");
            writer.append("Це ще один рядок.");
            System.out.println("Файл успішно записано!");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Вивід:

```
Файл успішно записано!
```

### Використовуючи Apache Commons IO

Бібліотека Apache Commons IO спрощує багато операцій, включаючи запис файлів. Ось як написати текст у файл за допомогою `FileUtils.writeStringToFile()`:

Спочатку додайте залежність до вашого проекту. Якщо використовуєте Maven, включіть:

```xml
<dependency>
  <groupId>org.apache.commons</groupId>
  <artifactId>commons-io</artifactId>
  <version>2.11.0</version> <!-- Перевірте останню версію -->
</dependency>
```

Потім використовуйте наступний код для запису тексту в файл:

```java
import org.apache.commons.io.FileUtils;
import java.io.File;
import java.io.IOException;

public class TextFileWriterCommonsIO {
    public static void main(String[] args) {
        try {
            FileUtils.writeStringToFile(new File("example.txt"), "Це текст, записаний з використанням Commons IO.", "UTF-8");
            System.out.println("Файл успішно записано!");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

```

Вивід:

```
Файл успішно записано!
```
