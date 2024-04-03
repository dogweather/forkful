---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:30.455269-07:00
description: "\u042F\u043A \u0437\u0440\u043E\u0431\u0438\u0442\u0438: #."
lastmod: '2024-03-13T22:44:49.110916-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\
  \u0442\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
weight: 24
---

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
