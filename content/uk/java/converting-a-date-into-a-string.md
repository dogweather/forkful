---
title:                "Java: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Чому

Багато разів трапляється так, що потрібно конвертувати дату в рядок, наприклад, для збереження в базі даних або відображення на екрані. Це корисно знати, як це зробити в програмі.

## Як

```Java
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateToString {
    public static void main(String[] args) {
        // Створення об'єкта для форматування дати
        SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");
        // Визначення дати
        Date date = new Date();
        // Конвертування дати в рядок
        String strDate = formatter.format(date);
        // Виведення результату
        System.out.println("Поточна дата у форматі рядка: " + strDate);
    }
}
```
Виведення:
```
Поточна дата у форматі рядка: 18/10/2021
```

## Deep Dive

У Java є вбудована бібліотека `java.text.SimpleDateFormat`, яка дозволяє здійснювати конвертацію дати в рядок в зручному форматі. У цьому прикладі, ми використовуємо `"dd/MM/yyyy"` для визначення шаблону форматування дати, де `dd` - день, `MM` - місяць, `yyyy` - рік. Інші ключові символи можуть бути використані для відображення інших частин дати, таких як години, хвилини, секунди. Докладніше про це можна дізнатися в [документації](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html) Java.

## Дивись також

- [Java SimpleDateFormat Example](https://www.javatpoint.com/java-simpledateformat)
- [Java Date to String Conversion](https://www.geeksforgeeks.org/date-to-string-conversion-in-java/)
- [Java Date and Time](https://www.w3schools.com/java/java_date.asp)