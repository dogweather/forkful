---
title:                "Склеивание строк"
aliases:
- ru/java/concatenating-strings.md
date:                  2024-01-28T23:56:33.031640-07:00
model:                 gpt-4-0125-preview
simple_title:         "Склеивание строк"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/java/concatenating-strings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?

Конкатенация строк означает их соединение встык, чтобы получить новую строку. Это удобно для создания пользовательских сообщений, формирования текста для вывода или обработки ввода пользователя.

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
