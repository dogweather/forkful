---
title:                "Створення тимчасового файлу"
html_title:           "C: Створення тимчасового файлу"
simple_title:         "Створення тимчасового файлу"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Що і Навіщо?

Створення тимчасового файлу - це розміщення даних на жорсткому диску в тимчасовому режимі. Програмісти роблять це для збереження даних, які потребуються протягом короткого періоду часу.

## Як це робиться:

Створення тимчасового файлу в Java можна зробити за допомогою методу `createTempFile()` класу `java.nio.file.Files`. Ось приклад:

```Java
import java.nio.file.*;

public class Main {
 public static void main(String[] args) {

   try {
     Path tempFilePath = Files.createTempFile(null, ".myTempFile");

     System.out.println("Temporary file created : " + tempFilePath.toString());

   } catch(Exception e) {
     e.printStackTrace();
   }
 }
}
```

Цей код створює тимчасовий файл та виводить його шлях. Ви отримаєте результат, який виглядає приблизно так:

```Java
Temporary file created : /tmp/2131231234.myTempFile
```

## Поглиблений огляд

Створення тимчасових файлів є старим when і достатньо часто використовуваним методом, коли дані мусить бути збережено на короткий термін. У минулому, кожна система мала свої методи для створення тимчасових файлів, але Java пропонує універсальний метод завдяки стандартному керуванню файлами.

Альтернативами можуть бути ін-меморі структури даних, такі як масиви або колекції, але вони залежать будуть від об'єму доступної пам'яті і тривалості сесії.

Важливо знати, що тимчасові файли створюються в каталозі, який визначається системою, і цей каталог може відрізнятися в залежності від системи.

## Додатково

Візьміть до уваги, що тимчасові файли можуть створювати проблеми з безпекою, якщо вони не видаляються. Ось пояснення цієї проблеми: [https://owasp.org/www-community/vulnerabilities/Insecure_Temporary_File](https://owasp.org/www-community/vulnerabilities/Insecure_Temporary_File)

Для поліпшення роботи з файлами Java пропонує клас `java.nio.file`, докладніше про який можна прочитати тут: [https://docs.oracle.com/javase/tutorial/essential/io/fileio.html](https://docs.oracle.com/javase/tutorial/essential/io/fileio.html)