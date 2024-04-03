---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:58:43.666547-07:00
description: "\u041A\u0430\u043A: Java \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\
  \u0435\u0442 \u0438\u0441\u043A\u043B\u044E\u0447\u0435\u043D\u0438\u044F \u0434\
  \u043B\u044F \u043E\u0431\u0440\u0430\u0431\u043E\u0442\u043A\u0438 \u043E\u0448\
  \u0438\u0431\u043E\u043A. \u0412\u044B \u043E\u043A\u0440\u0443\u0436\u0430\u0435\
  \u0442\u0435 \u0440\u0438\u0441\u043A\u043E\u0432\u0430\u043D\u043D\u044B\u0439\
  \ \u043A\u043E\u0434 \u0431\u043B\u043E\u043A\u043E\u043C `try` \u0438 \u043B\u043E\
  \u0432\u0438\u0442\u0435 \u0438\u0441\u043A\u043B\u044E\u0447\u0435\u043D\u0438\u044F\
  \ \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\u044E `catch`. \u0412\u043E\u0442\
  \ \u043F\u0440\u043E\u0441\u0442\u043E\u0439 \u043F\u0440\u0438\u043C\u0435\u0440\
  ."
lastmod: '2024-03-13T22:44:44.835559-06:00'
model: gpt-4-0125-preview
summary: "Java \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442 \u0438\
  \u0441\u043A\u043B\u044E\u0447\u0435\u043D\u0438\u044F \u0434\u043B\u044F \u043E\
  \u0431\u0440\u0430\u0431\u043E\u0442\u043A\u0438 \u043E\u0448\u0438\u0431\u043E\u043A\
  ."
title: "\u041E\u0431\u0440\u0430\u0431\u043E\u0442\u043A\u0430 \u043E\u0448\u0438\u0431\
  \u043E\u043A"
weight: 16
---

## Как:
Java использует исключения для обработки ошибок. Вы окружаете рискованный код блоком `try` и ловите исключения с помощью `catch`. Вот простой пример:

```java
public class ErrorHandlingExample {
    public static void main(String[] args) {
        try {
            int result = divide(10, 0);
            System.out.println("Результат: " + result);
        } catch (ArithmeticException e) {
            System.out.println("Упс, на ноль делить нельзя!");
        }
    }

    private static int divide(int числитель, int знаменатель) {
        return числитель / знаменатель;
    }
}
```

Вывод:
```
Упс, на ноль делить нельзя!
```

## Подробнее
Обработка ошибок в Java развивалась. В начале не было исключений; программисты проверяли коды ошибок. Затем Java ввела блоки try-catch, что позволило более элегантно обрабатывать ошибки.

Альтернативы традиционному `try-catch` включают `try-with-resources` для автоматического закрытия ресурсов и более чистого кода, введенные в Java 7.

Детали реализации имеют значение. Например, перехват `Exception` или `Throwable` обычно является плохой практикой. Это слишком общо, что может маскировать баги, о которых вы можете не знать. Ограничивайтесь конкретными исключениями.

## Смотрите также
- Официальные учебные пособия Oracle по исключениям в Java: [https://docs.oracle.com/javase/tutorial/essential/exceptions/](https://docs.oracle.com/javase/tutorial/essential/exceptions/)
- Документация Java по оператору `try-with-resources`: [https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html](https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html)
- Effective Java от Джошуа Блоха, для лучших практик по исключениям.
