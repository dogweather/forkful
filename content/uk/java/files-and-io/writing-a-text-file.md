---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:30.455269-07:00
description: "\u0417\u0430\u043F\u0438\u0441 \u0442\u0435\u043A\u0441\u0442\u043E\u0432\
  \u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443 \u0432 Java \u043F\u043E\u043B\
  \u044F\u0433\u0430\u0454 \u0443 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\
  \u0430\u043D\u043D\u0456 \u043C\u043E\u0436\u043B\u0438\u0432\u043E\u0441\u0442\u0435\
  \u0439 \u043C\u043E\u0432\u0438 \u0434\u043B\u044F \u0441\u0442\u0432\u043E\u0440\
  \u0435\u043D\u043D\u044F \u0442\u0430 \u0437\u0430\u043F\u0438\u0441\u0443 \u0432\
  \u043C\u0456\u0441\u0442\u0443 \u0443 \u0444\u0430\u0439\u043B\u0438 \u043D\u0430\
  \ \u0444\u0430\u0439\u043B\u043E\u0432\u0456\u0439 \u0441\u0438\u0441\u0442\u0435\
  \u043C\u0456. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438\
  \ \u0440\u043E\u0431\u043B\u044F\u0442\u044C \u0446\u0435 \u0437\u2026"
lastmod: '2024-03-11T00:14:22.961471-06:00'
model: gpt-4-0125-preview
summary: "\u0417\u0430\u043F\u0438\u0441 \u0442\u0435\u043A\u0441\u0442\u043E\u0432\
  \u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443 \u0432 Java \u043F\u043E\u043B\
  \u044F\u0433\u0430\u0454 \u0443 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\
  \u0430\u043D\u043D\u0456 \u043C\u043E\u0436\u043B\u0438\u0432\u043E\u0441\u0442\u0435\
  \u0439 \u043C\u043E\u0432\u0438 \u0434\u043B\u044F \u0441\u0442\u0432\u043E\u0440\
  \u0435\u043D\u043D\u044F \u0442\u0430 \u0437\u0430\u043F\u0438\u0441\u0443 \u0432\
  \u043C\u0456\u0441\u0442\u0443 \u0443 \u0444\u0430\u0439\u043B\u0438 \u043D\u0430\
  \ \u0444\u0430\u0439\u043B\u043E\u0432\u0456\u0439 \u0441\u0438\u0441\u0442\u0435\
  \u043C\u0456. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438\
  \ \u0440\u043E\u0431\u043B\u044F\u0442\u044C \u0446\u0435 \u0437\u2026"
title: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\
  \u0442\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
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
