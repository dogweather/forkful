---
title:    "Java: Написання текстового файлу"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Чому

Написання текстового файлу - важлива частина програмування, оскільки воно дозволяє зберігати дані, щоб вони могли бути використані в майбутніх програмах. Використання текстових файлів в програмуванні також дозволяє зберігати дані на постійному носії, що може бути корисним для резервного копіювання або обміну з іншими користувачами.

## Як

В Java є кілька різних способів написання текстових файлів, але найпростішим і найбільш ефективним є використання класу ```File``` та об'єкту ```BufferedWriter```. Ось приклад коду, який демонструє цей процес:

```
import java.io.*;

public class WriteFile {

    public static void main(String[] args) {

        // Ініціалізація об'єкту File та BufferedWriter
        File file = new File("myFile.txt");
        BufferedWriter writer = null;

        try {
            // Відкриття файлу
            writer = new BufferedWriter(new FileWriter(file));

            // Запис тексту в файл
            writer.write("Це якась інформація, яку ми бажаємо зберегти.");

            // Очищення буферу та збереження змін
            writer.flush();

            // Виведення повідомлення про успішне завершення
            System.out.println("Текст успішно записаний в файл.");

        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            try {
                // Закриття з'єднання
                writer.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}
```

Отриманий результат у файлі ```myFile.txt``` буде виглядати наступним чином:

```
Це якась інформація, яку ми бажаємо зберегти.
```

## Глибокий занурення

Існують різні параметри, які можуть бути використані для налаштування процесу записування в файл за допомогою класу ```BufferedWriter```. Наприклад, за допомогою методу ```writer.newLine()``` можна додавати порожні рядки у текстовий файл, щоб його було більш зручно читати. Також, можна використовувати різні методи для форматування тексту, як-от ```writer.write(String.format("Количество товаров: %d", кількість));```, який додає форматований рядок у файл з вказаною кількістю товарів.

## Дивіться також

- [Java File клас](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- [Java BufferedWriter клас](https://docs.oracle.com/javase/7/docs/api/java/io/BufferedWriter.html)
- [Java String.format() метод](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#format(java.lang.String,%20java.lang.Object...))