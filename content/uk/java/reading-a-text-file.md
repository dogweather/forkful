---
title:                "Java: Читання текстового файлу"
simple_title:         "Читання текстового файлу"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Чому

Написання та редагування текстових файлів є важливою частиною програмування на Java. Це дозволяє зберігати дані та налаштовувати програми для різних вхідних даних.

## Як

Незалежно від того, чи ви створюєте новий текстовий файл або читаєте існуючий, Java має вбудований функціонал для роботи з текстовими файлами.

Розглянемо приклад на читання текстового файлу та виведення його змісту в консоль:

```Java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class ReadFileExample {

    public static void main(String[] args) {

        try (BufferedReader br = new BufferedReader(new FileReader("filename.txt"))) {

            String line;
            while ((line = br.readLine()) != null) {
                System.out.println(line);
            }

        } catch (IOException e) {
            System.out.println("Error reading file: " + e.getMessage());
        }
    }
}
```

Вхідний файл має такий вміст:

```
Hello world!
```

Після виконання коду, в консолі ми побачимо:

```
Hello world!
```

Також можна використовувати більш продвинуті методи, такі як `Scanner`, для читання та обробки вхідного тексту. Для детальнішої інформації про читання та записування текстових файлів на Java, рекомендуємо ознайомитися з документацією.

## Глибоке погруження

Під час читання текстового файлу, необхідно врахувати можливі помилки та винятки, що можуть виникнути. Наприклад, якщо файл не знайдено або містить некоректний формат даних.

Крім того, важливо правильно закрити потоки після завершення читання, щоб уникнути витоків пам'яті. Для цього можна використати конструкцію `try-with-resources`, яка автоматично закриє поток у випадку помилки чи успішного виконання коду.

Перевіримо, чи існує файл перед читанням:

```Java
import java.io.File;

public class FileExistExample {

    public static void main(String[] args) {

        File file = new File("filename.txt");
        if(file.exists()) {
            System.out.println("File exists!");
        
            // код для читання файла
        } else {
            System.out.println("File does not exist.");
        }
    }
}
```

Виходячи з наведеного прикладу, можна зрозуміти, що перед початком роботи з текстовими файлами, потрібно заздалегідь врахувати можливі ризики та проблеми, які можуть виникнути під час читання та обробки даних.

## Дивись також

- [Документація Java для роботи з текстовими файлами](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
- [Стаття про роботу з текстовими файлами на Java](https://www.baeldung.com/java-write-to-file)