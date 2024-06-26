---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:58:12.694265-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0418\u0437\u0432\u043B\u0435\u0447\u0435\u043D\u0438\u0435 \u043F\
  \u043E\u0434\u0441\u0442\u0440\u043E\u043A\u0438 \u0432 Java \u043F\u0440\u043E\u0441\
  \u0442\u043E \u0441 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\
  \u043D\u0438\u0435\u043C \u043C\u0435\u0442\u043E\u0434\u0430 `substring`. \u0412\
  \u043E\u0442 \u043A\u0430\u043A \u044D\u0442\u043E \u0434\u0435\u043B\u0430\u0435\
  \u0442\u0441\u044F."
lastmod: '2024-03-13T22:44:44.799276-06:00'
model: gpt-4-0125-preview
summary: "\u0418\u0437\u0432\u043B\u0435\u0447\u0435\u043D\u0438\u0435 \u043F\u043E\
  \u0434\u0441\u0442\u0440\u043E\u043A\u0438 \u0432 Java \u043F\u0440\u043E\u0441\u0442\
  \u043E \u0441 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\
  \u0438\u0435\u043C \u043C\u0435\u0442\u043E\u0434\u0430 `substring`."
title: "\u0418\u0437\u0432\u043B\u0435\u0447\u0435\u043D\u0438\u0435 \u043F\u043E\u0434\
  \u0441\u0442\u0440\u043E\u043A"
weight: 6
---

## Как это сделать:
Извлечение подстроки в Java просто с использованием метода `substring`. Вот как это делается:

```java
public class SubstringExample {
    public static void main(String[] args) {
        String fullString = "Привет, Мир!";

        // Извлечь с индекса 7 до конца строки
        String sub1 = fullString.substring(7);
        System.out.println(sub1); // Вывод: Мир!

        // Извлечь с индекса 0 до индекса 4 (5 не включается)
        String sub2 = fullString.substring(0, 5);
        System.out.println(sub2); // Вывод: Привет
    }
}
```

**Помните**: В Java индексация строк начинается с 0.

## Глубже в Тему
Метод `substring` существует с ранних версий Java, предлагая простой способ получения частей строки. В старых версиях Java `substring` использовал общий массив символов оригинальной строки, что могло привести к утечкам памяти, если оригинальная строка была большой, а подстрока сохранялась долгое время. Начиная с Java 7 обновление 6, `substring` создает новую строку, так что старая может быть собрана сборщиком мусора, если не используется где-либо еще.

Также, прежде чем использовать `substring`, рассмотрите возможность использования `split`, `replace` или утилиты регулярных выражений для более сложных сценариев. Внутри, `substring` в Java использует методы из класса `String`, которые копируют массивы — эффективно, но без вашего прямого контроля.

## Смотрите также
- Для полного представления о том, что вы можете делать со строками в Java, посмотрите документацию класса `String`: [String (Java SE 15 & JDK 15)](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html)
- Погружение в более сложные манипуляции со строками? Классы `Pattern` и `Matcher` вашими союзниками: [Pattern (Java SE 15 & JDK 15)](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/util/regex/Pattern.html)
- Учебник по использованию регулярных выражений в Java: [Регулярные Выражения](https://docs.oracle.com/javase/tutorial/essential/regex/)

Будь то простая обрезка или сложное извлечение данных, необходимые функции у вас под рукой. Держите ваш инструментарий понятным и готовым к использованию.
