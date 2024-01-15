---
title:                "Перетворення дати в рядок"
html_title:           "Java: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Для чого
Існує безліч причин, чому ви може бажати конвертувати дату на Java в рядок. Наприклад, щоб зберегти її в базі даних, вивести користувачам або передати дані на зовнішній сервіс.

## Як це зробити
Конвертація дати в рядок можлива за допомогою класу "SimpleDateFormat". Спочатку створіть об'єкт цього класу з необхідним форматом дати, наприклад "dd/MM/yyyy". Потім використовуйте його метод "format()", якому передайте дату, яку потрібно конвертувати. Також, обов'язково використовуйте блок "try-catch" для обробки можливих помилок.

```Java
import java.text.SimpleDateFormat;
import java.util.Date;

public class Main {
    public static void main(String[] args) {
        SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");
        
        try {
            Date date = new Date();
            String stringDate = dateFormat.format(date);
            System.out.println(stringDate);
        } catch (Exception e) {
            e.printStackTrace();
        }
        
        // Output: 23/03/2021
    }
}
```

## Глибші деталі
Клас "SimpleDateFormat" дозволяє вказувати будь-який формат дати, що ви забажаєте, наприклад "yyyy-MM-dd" для американського стандарту. Також, цей клас може використовуватися для парсингу рядка у дату за заданим форматом. Детальну інформацію можна знайти в документації Java.

## Дивіться також
- [Java SimpleDateFormat документація](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Java Date та Calendar документація](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Стаття про роботу з датами на Java](https://www.baeldung.com/java-date-time)