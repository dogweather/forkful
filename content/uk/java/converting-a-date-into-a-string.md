---
title:                "Перетворення дати в рядок"
html_title:           "Lua: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Що це і навіщо?

Перетворення дати в рядок - це перетворення об'єкта `java.util.Date` на рядок у форматі, що розуміє людина, використовуючи `java.text.SimpleDateFormat`. Навіщо? Щоб зобразити дату у більш зручному для нас фомраті, або зберегти у файл чи в базу даних.

## Як це зробити:

Нижче наведений приклад коду та результат його виконання:

```Java
import java.text.SimpleDateFormat;
import java.util.Date;

public class Main {
public static void main(String[] args) {
        Date date = new Date();
        SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy HH:mm:ss");
        String dateAsString = format.format(date);
        System.out.println(dateAsString);
    }
}
```

Виконання цього коду виведе поточну дату та час, наприклад: `23-05-2022 10:20:30`.

## Детальніше:

У минулому, коли ми працювали з датами у `java.util.Date`, ми здебільшого використовували `java.text.DateFormat`, що має простіше інтерфейс. Але, через його малу гнучкість та можливість виникнення помилок потоків, був розроблений `java.text.SimpleDateFormat`. Його можна налаштувати так, як вам потрібно, але він дещо повільніший через більш складну реалізацію.

В якості альтернативи, ви можете використати `java.time.format.DateTimeFormatter` з `java.time.LocalDateTime`, який був доданий у Java 8. Він належить до нової бібліотеки часу та дати, що є більш потужною та надійною ніж старі `java.util.Date` та `java.text.SimpleDateFormat`.

## Також дивіться:

Для докладнішої інформації та прикладів, дивіться:

- [Oracle Java Documentation](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Java 8 DateTime API Guide](https://www.baeldung.com/java-8-date-time-intro)
- [SimpleDateFormat Example](https://mkyong.com/java/java-date-and-calendar-examples/)