---
title:                "Інтерполяція рядка"
html_title:           "Java: Інтерполяція рядка"
simple_title:         "Інтерполяція рядка"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Що це та навіщо це потрібно?

Інтерполяція рядка - це простий метод, яким програміст формує рядок, вставляючи в нього змінні цінності. Це робить ваш код більш чистим і зрозумілим.

## Як це зробити:

У Java 15 було запроваджено новий API для інтерполяції рядків. Ви можете використовувати `String.format()` для цього.

```Java
public class Main {
    public static void main(String[] args) {
        int age = 30;
        String name = "Ivan";
        String output = String.format("Привіт. Мене звати %s і мені %d років.", name, age);
        System.out.println(output);
    }
}
```

Цей код виведе: "Привіт. Мене звати Ivan і мені 30 років."

## Поглиблений огляд:

1. Історичний контекст: Інтерполяція рядків була запроваджена в багатьох мовах програмування ще до Java. Проте, Java додала цю опцію лише у версії 15.
2. Альтернативи: Альтернативою `String.format()` є `MessageFormat.format()`, який також дозволяє інтерполювати рядки, але використовує різний синтаксис.
3. Детальніше про реалізацію: У Java інтерполяція рядків досягається через використання класу `Formatter`, який є основою `String.format()`.

## Ще декілька ресурсів:

Для додаткового вивчення щодо інтерполяції рядків в Java ви можете взяти до уваги наступні ресурси:
- [Java String Format Examples](https://dzone.com/articles/java-string-format-examples)
- [Oracle Java Documentation](https://docs.oracle.com/javase/7/docs/api/java/util/Formatter.html)