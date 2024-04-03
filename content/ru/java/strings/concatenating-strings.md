---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:56:33.031640-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412\u043E\u0442 \u043A\u0440\u0430\u0442\u043A\u043E\u0435 \u0440\
  \u0443\u043A\u043E\u0432\u043E\u0434\u0441\u0442\u0432\u043E \u043F\u043E \u043A\
  \u043E\u043D\u043A\u0430\u0442\u0435\u043D\u0430\u0446\u0438\u0438 \u0441\u0442\u0440\
  \u043E\u043A \u0432 Java."
lastmod: '2024-03-13T22:44:44.805078-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u043E\u0442 \u043A\u0440\u0430\u0442\u043A\u043E\u0435 \u0440\u0443\
  \u043A\u043E\u0432\u043E\u0434\u0441\u0442\u0432\u043E \u043F\u043E \u043A\u043E\
  \u043D\u043A\u0430\u0442\u0435\u043D\u0430\u0446\u0438\u0438 \u0441\u0442\u0440\u043E\
  \u043A \u0432 Java."
title: "\u0421\u043A\u043B\u0435\u0438\u0432\u0430\u043D\u0438\u0435 \u0441\u0442\u0440\
  \u043E\u043A"
weight: 3
---

## Как это сделать:
Вот краткое руководство по конкатенации строк в Java:

```java
public class StringConcatenationDemo {
    public static void main(String[] args) {
        String firstName = "John";
        String lastName = "Doe";
        
        // Использование оператора плюс
        String fullName = firstName + " " + lastName;
        System.out.println(fullName); // Вывод: John Doe
        
        // Использование метода concat()
        String anotherFullName = firstName.concat(" ").concat(lastName);
        System.out.println(anotherFullName); // Вывод: John Doe
        
        // Использование StringBuilder для множественной конкатенации
        StringBuilder builder = new StringBuilder();
        builder.append(firstName).append(" ").append(lastName);
        System.out.println(builder.toString()); // Вывод: John Doe
    }
}
```

## Глубокое Погружение
Казалось бы, конкатенация строк проста, правда? Она существует в Java с самого начала, и у нас есть несколько способов её выполнить. В ранних версиях Java под капотом для простой конкатенации с помощью `+` использовался StringBuilder. Затем появилась Java 5, и эффективность выросла благодаря внедрению `StringJoiner` и дальнейшему улучшению класса `StringBuilder`.

Теперь вы можете задаться вопросом, почему бы не использовать оператор `+`, если это то же самое? Оказывается, `+` отлично подходит для быстрой работы с небольшими строками или несколькими конкатенациями. Однако, за кулисами это может стать затратным с точки зрения производительности, если использовать его в цикле с большим количеством итераций, поскольку это создает временные объекты, прежде чем будет достигнута конечная версия строки.

В этих случаях на тяжёлой работе вступают `StringBuilder` или `StringBuffer`. `StringBuilder`, как правило, быстрее из-за отсутствия синхронизации — делая его небезопасным для потоков, но быстрым. `StringBuffer` — это более старый, потокобезопасный вариант. Он медленнее из-за накладных расходов на синхронизацию. Выбирайте на основе ваших потребностей в безопасности потоков.

Что касается метода `concat()`, он прост в использовании, но не так гибок, как `StringBuilder`. Хотите циклически добавлять ещё больше строк? С `concat()` это менее удобно.

Начиная с Java 8 и далее, у нас также есть `String.join()`, который весьма удобен для соединения коллекций строк с разделителем.

## Смотрите также
- [Документация по классу `String`](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- [Документация по классу `StringBuilder`](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/StringBuilder.html)
- [Документация по классу `StringBuffer`](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/StringBuffer.html)
- [Учебники Oracle по строкам в Java](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
