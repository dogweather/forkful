---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:44.447926-07:00
description: "\u0417\u0430\u043F\u0438\u0441\u044C \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430 \u0432 Java \u043E\u0437\
  \u043D\u0430\u0447\u0430\u0435\u0442 \u0441\u043E\u0445\u0440\u0430\u043D\u0435\u043D\
  \u0438\u0435 \u0442\u0435\u043A\u0441\u0442\u043E\u0432\u044B\u0445 \u0434\u0430\
  \u043D\u043D\u044B\u0445 \u0432 \u0444\u0430\u0439\u043B \u043D\u0430 \u0434\u0438\
  \u0441\u043A\u0435. \u0420\u0430\u0437\u0440\u0430\u0431\u043E\u0442\u0447\u0438\
  \u043A\u0438 \u0434\u0435\u043B\u0430\u044E\u0442 \u044D\u0442\u043E \u0434\u043B\
  \u044F \u0437\u0430\u0434\u0430\u0447, \u0442\u0430\u043A\u0438\u0445 \u043A\u0430\
  \u043A \u043B\u043E\u0433\u0438\u0440\u043E\u0432\u0430\u043D\u0438\u0435,\u2026"
lastmod: '2024-03-13T22:44:44.856937-06:00'
model: gpt-4-0125-preview
summary: "\u0417\u0430\u043F\u0438\u0441\u044C \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430 \u0432 Java \u043E\u0437\
  \u043D\u0430\u0447\u0430\u0435\u0442 \u0441\u043E\u0445\u0440\u0430\u043D\u0435\u043D\
  \u0438\u0435 \u0442\u0435\u043A\u0441\u0442\u043E\u0432\u044B\u0445 \u0434\u0430\
  \u043D\u043D\u044B\u0445 \u0432 \u0444\u0430\u0439\u043B \u043D\u0430 \u0434\u0438\
  \u0441\u043A\u0435."
title: "\u0421\u043E\u0437\u0434\u0430\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\
  \u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430"
weight: 24
---

## Как:
С использованием пакета `java.nio.file` в Java, запись в текстовый файл становится простой. Проверьте `Files.write()` для быстрого сохранения:

```java
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class WriteTextFileExample {
    public static void main(String[] args) {
        List<String> lines = List.of("Строка 1", "Строка 2", "Строка 3");
        Path file = Path.of("example.txt");

        try {
            Files.write(file, lines);
            System.out.println("Успешно записано в файл.");
        } catch (IOException e) {
            System.err.println("Ой! Произошла ошибка: " + e.getMessage());
        }
    }
}
```

Вывод:
```
Успешно записано в файл.
```

## Глубже:
В былые дни Java I/O целиком была связана с `FileWriter` и `BufferedWriter`. Теперь пакет NIO (`java.nio.file`) стал основным. `Files.write()` удобен — обрабатывает создание, открытие и запись за один раз. Альтернатива? `FileOutputStream` для контроля на уровне байт. Внутри, `Files.write()` использует `BufferedWriter` и `Charset` для кодирования текста в байты.

## Смотрите также
Углубитесь в файловый ввод/вывод с этими ссылками:

- Официальная документация `java.nio.file.Files`: https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html
- Руководство Oracle по файловому вводу/выводу: https://docs.oracle.com/javase/tutorial/essential/io/
- Для подхода, ориентированного на байты, изучите `FileOutputStream`: https://docs.oracle.com/javase/8/docs/api/java/io/FileOutputStream.html
