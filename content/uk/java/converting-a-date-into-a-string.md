---
title:    "Java: Перетворення дати у рядок."
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Чому
Перетворення дати в рядок є корисною функцією для представлення дати у зрозумілому форматі для користувача. Вона також є важливою для оперування та порівняння дат у програмах.

## Як
```Java
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateToStringExample{
    public static void main(String[] args) {
        //створення об'єкту за допомогою поточної дати
        Date currentDate = new Date();
        //створення формату для перетворення
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd");
        //перетворення дати в рядок
        String stringDate = dateFormat.format(currentDate);
        //виведення результату
        System.out.println("Поточна дата у форматі рядка: " + stringDate);
    }
}
```
**Вихід:**
```
Поточна дата у форматі рядка: 2020/06/11
```

## Глибокий аналіз
У Java є ряд вбудованих класів для роботи з датами, включаючи **Date** та **SimpleDateFormat**. *Date* представляє поточну дату та час, а *SimpleDateFormat* дозволяє встановити необхідний формат для перетворення дати. Крім *yyyy/MM/dd*, існує також багато інших шаблонів для форматування дати, які можна використовувати залежно від потреби.

## Дивіться також
- [Java Date у деталях](https://www.w3schools.com/java/java_date.asp)
- [Приклади форматування дати у Java](https://www.javatpoint.com/java-simpledateformat)