---
title:    "Java: Створення тимчасового файлу"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Чому
Створення тимчасового файлу є важливою частиною розробки програмного забезпечення. Він дозволяє зберігати тимчасові дані під час виконання програми та після закінчення роботи видаляє їх для оптимізації пам'яті та простору на диску.

## Як створити тимчасовий файл
Для створення тимчасового файлу в Java існують два основних методи - `createTempFile` та `createTempDirectory`. Перший для створення файлу, а другий для створення директорії. Давайте розглянемо приклад створення тимчасового файлу з використанням методу `createTempFile`.

```Java
import java.io.File;
import java.io.IOException;

public class TempFileExample {

    public static void main(String[] args) {
        try {
            // створюємо тимчасовий файл з префіксом "tmp" та розширенням ".txt"
            File tempFile = File.createTempFile("tmp", ".txt");

            // виводимо шлях до цього файлу
            System.out.println("Шлях до тимчасового файлу: " + tempFile.getAbsolutePath());

            // встановлюємо властивості файлу: можливість читання, запису та видалення
            tempFile.setReadable(true);
            tempFile.setWritable(true);
            tempFile.setExecutable(false);

            // видаляємо тимчасовий файл після закінчення роботи програми
            tempFile.deleteOnExit();

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Після запуску цього коду, у консолі ми побачимо шлях до створеного тимчасового файлу, наприклад:
```
Шлях до тимчасового файлу: C:\Users\User\AppData\Local\Temp\tmp621681609530660909.txt
```

## Детальніше про створення тимчасового файлу
Під час створення тимчасового файлу, Java створює файл у визначеному системою шляху до тимчасової папки. За замовчуванням, це є папка `AppData\Local\Temp` у Windows або `/tmp` у Unix. Ми також можемо вказати директорію у якій потрібно створити тимчасовий файл, передаючи шлях до неї у вигляді аргументу у метод `createTempFile`.

Також, варто зазначити, що створений тимчасовий файл унікальний. Це означає, що виконуючи код знову та знову, ми отримуємо різні тимчасові файли.

## Дивись також
- [Офіційна документація Java про метод `createTempFile`](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html#createTempFile(java.lang.String,java.lang.String,java.io.File))
- [Стаття про створення тимчасових файлів в Java на сайті JournalDev](https://www.journaldev.com/17149/java-create-temporary-file)
- [Зберігання тимчасових файлів у Java на