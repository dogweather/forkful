---
date: 2024-01-26 00:54:05.670124-07:00
description: "\u042F\u043A: Java \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\
  \u0432\u0443\u0454 \u0432\u0438\u043D\u044F\u0442\u043A\u0438 \u0434\u043B\u044F\
  \ \u043E\u0431\u0440\u043E\u0431\u043A\u0438 \u043F\u043E\u043C\u0438\u043B\u043E\
  \u043A. \u0412\u0438 \u043E\u0442\u043E\u0447\u0443\u0454\u0442\u0435 \u0440\u0438\
  \u0437\u0438\u043A\u043E\u0432\u0430\u043D\u0438\u0439 \u043A\u043E\u0434 \u0431\
  \u043B\u043E\u043A\u043E\u043C `try` \u0442\u0430 \u043B\u043E\u0432\u0438\u0442\
  \u0435 \u0432\u0438\u043D\u044F\u0442\u043A\u0438 \u0437\u0430 \u0434\u043E\u043F\
  \u043E\u043C\u043E\u0433\u043E\u044E `catch`. \u041E\u0441\u044C \u043F\u0440\u043E\
  \u0441\u0442\u0438\u0439 \u043F\u0440\u0438\u043A\u043B\u0430\u0434."
lastmod: '2024-03-13T22:44:49.092345-06:00'
model: gpt-4-1106-preview
summary: "Java \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0454\
  \ \u0432\u0438\u043D\u044F\u0442\u043A\u0438 \u0434\u043B\u044F \u043E\u0431\u0440\
  \u043E\u0431\u043A\u0438 \u043F\u043E\u043C\u0438\u043B\u043E\u043A."
title: "\u041E\u0431\u0440\u043E\u0431\u043A\u0430 \u043F\u043E\u043C\u0438\u043B\u043E\
  \u043A"
weight: 16
---

## Як:
Java використовує винятки для обробки помилок. Ви оточуєте ризикований код блоком `try` та ловите винятки за допомогою `catch`. Ось простий приклад:

```java
public class ErrorHandlingExample {
    public static void main(String[] args) {
        try {
            int result = divide(10, 0);
            System.out.println("Результат: " + result);
        } catch (ArithmeticException e) {
            System.out.println("Ой, на нуль ділити не можна!");
        }
    }

    private static int divide(int numerator, int denominator) {
        return numerator / denominator;
    }
}
```

Вивід:
```
Ой, на нуль ділити не можна!
```

## Поглиблений Розгляд
Обробка помилок у Java розвивалася. На початку не було винятків; програмісти перевіряли коди помилок. Потім Java ввела блоки try-catch, що дозволили більш елегантно обробляти помилки.

Альтернативи традиційному `try-catch` включають `try-with-resources` для автоматичного закриття ресурсів та чистішого коду, представленого в Java 7.

Деталі реалізації мають значення. Наприклад, ловіння `Exception` або `Throwable` зазвичай є поганою практикою. Це занадто широко, що може приховувати помилки, про які ви навіть не здогадуєтесь. Тримайтесь конкретних винятків.

## Дивіться також
- Офіційні навчальні посібники Oracle по Java про винятки: [https://docs.oracle.com/javase/tutorial/essential/exceptions/](https://docs.oracle.com/javase/tutorial/essential/exceptions/)
- Документація Java по оператору `try-with-resources`: [https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html](https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html)
- Книга "Ефективна Java" авторства Джошуа Блоха, для найкращих практик щодо винятків.
