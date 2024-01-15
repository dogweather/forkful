---
title:                "Написання текстового файлу"
html_title:           "Java: Написання текстового файлу"
simple_title:         "Написання текстового файлу"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Тому що

Написання текстового файлу є необхідною частиною програмування в Java, оскільки це дозволяє зберігати дані та результати виконання програми на диску для подальшого використання.

## Як це зробити

Найпростіший спосіб написати текстовий файл в Java - це використовувати клас `FileWriter`. Для початку слід створити об'єкт цього класу та передати йому шлях до файлу для записування даних:

```Java
FileWriter writer = new FileWriter("myFile.txt");
```

Далі можна використовувати метод `write()` для додавання тексту до файлу:

```Java
writer.write("Це текст, який буде збережений у файлі.");
```

Коли всі дані додані до файлу, необхідно закрити об'єкт `FileWriter` за допомогою методу `close()`:

```Java
writer.close();
```

#### Приклад

```Java
import java.io.FileWriter;

public class WritingTextFile {
    public static void main(String[] args) throws Exception {
        FileWriter writer = new FileWriter("myFile.txt");
        writer.write("Це приклад написання текстового файлу в Java.");
        writer.close();
    }
}
```

#### Вихідний файл `myFile.txt`

Це приклад написання текстового файлу в Java.

## Глибше про написання текстових файлів

### Класи `Writer` та `OutputStreamWriter`

Крім `FileWriter`, в Java є ще два класи, які дозволяють записувати тексові дані до файлу - `Writer` та `OutputStreamWriter`. Ці класи дозволяють зберігати дані в різних форматах, наприклад у UTF-8 або UTF-16.

### Використання буферу

Використання буферу може покращити продуктивність записування текстового файлу. Для цього слід додати буферний об'єкт до екземпляру `Writer` або `OutputStreamWriter`:

```Java
BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter("myFile.txt"));
```

Після цього можна використовувати метод `write()` для додавання даних до буфера, а метод `flush()` щоб записати дані із буферу до файлу.

## Додатковий матеріал

[Офіційна документація Java - Клас FileWriter](https://docs.oracle.com/javase/10/docs/api/java/io/FileWriter.html)

[Стаття про роботу з файлами в Java на сайті Tutorials Point](https://www.tutorialspoint.com/java/java_files_io.htm)

[Відеоурок з написання текстових файлів в Java на YouTube](https://www.youtube.com/watch?v=ku6cgvNLkR0)

## Дивіться також

[Як читати текстовий файл в Java](https://github.com/OlyaLevchuk/Java-Text-File-Reader-Article-Ukrainian)

[Список методів для роботи з текстовим файлом у Java](https://github.com/OlyaLevchuk/Java-String-File-Methods-Article-Ukrainian)

[Курс Java - Основи програмування на YouTube](https://www.youtube.com/playlist?list=PLZYrS7Oes447a5IftMjImPfDAw41nOOSa)