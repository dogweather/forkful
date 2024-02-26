---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:30.393661-07:00
description: "\u041F\u043E\u0438\u0441\u043A \u0438 \u0437\u0430\u043C\u0435\u043D\
  \u0430 \u0442\u0435\u043A\u0441\u0442\u0430 \u0432 Java \u043F\u043E\u0437\u0432\
  \u043E\u043B\u044F\u044E\u0442 \u043F\u0435\u0440\u0435\u043F\u0438\u0441\u044B\u0432\
  \u0430\u0442\u044C \u043E\u0440\u0438\u0433\u0438\u043D\u0430\u043B\u044C\u043D\u044B\
  \u0435 \u0441\u0442\u0440\u043E\u043A\u0438 \u043D\u043E\u0432\u044B\u043C\u0438\
  \ \u0441\u0438\u043C\u0432\u043E\u043B\u0430\u043C\u0438 - \u043F\u0440\u0435\u0434\
  \u0441\u0442\u0430\u0432\u044C\u0442\u0435 \u044D\u0442\u043E \u043A\u0430\u043A\
  \ \u0446\u0438\u0444\u0440\u043E\u0432\u043E\u0439 \u043A\u043E\u0440\u0440\u0435\
  \u043A\u0442\u043E\u0440. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\
  \u0441\u0442\u044B \u0447\u0430\u0441\u0442\u043E\u2026"
lastmod: '2024-02-25T18:49:42.454969-07:00'
model: gpt-4-0125-preview
summary: "\u041F\u043E\u0438\u0441\u043A \u0438 \u0437\u0430\u043C\u0435\u043D\u0430\
  \ \u0442\u0435\u043A\u0441\u0442\u0430 \u0432 Java \u043F\u043E\u0437\u0432\u043E\
  \u043B\u044F\u044E\u0442 \u043F\u0435\u0440\u0435\u043F\u0438\u0441\u044B\u0432\u0430\
  \u0442\u044C \u043E\u0440\u0438\u0433\u0438\u043D\u0430\u043B\u044C\u043D\u044B\u0435\
  \ \u0441\u0442\u0440\u043E\u043A\u0438 \u043D\u043E\u0432\u044B\u043C\u0438 \u0441\
  \u0438\u043C\u0432\u043E\u043B\u0430\u043C\u0438 - \u043F\u0440\u0435\u0434\u0441\
  \u0442\u0430\u0432\u044C\u0442\u0435 \u044D\u0442\u043E \u043A\u0430\u043A \u0446\
  \u0438\u0444\u0440\u043E\u0432\u043E\u0439 \u043A\u043E\u0440\u0440\u0435\u043A\u0442\
  \u043E\u0440. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\
  \u044B \u0447\u0430\u0441\u0442\u043E\u2026"
title: "\u041F\u043E\u0438\u0441\u043A \u0438 \u0437\u0430\u043C\u0435\u043D\u0430\
  \ \u0442\u0435\u043A\u0441\u0442\u0430"
---

{{< edit_this_page >}}

## Что и почему?

Поиск и замена текста в Java позволяют переписывать оригинальные строки новыми символами - представьте это как цифровой корректор. Программисты часто используют это для очистки данных, настройки параметров или адаптации сообщений.

## Как это сделать:

Поиск и замена в Java – это просто благодаря классу `String` и его методу `replace()`. Вот как это делается:

```java
public class ReplaceDemo {
    public static void main(String[] args) {
        String originalText = "The quick brown fox jumps over the lazy dog";
        String modifiedText = originalText.replace("lazy", "energetic");
        
        System.out.println("До: " + originalText);
        System.out.println("После: " + modifiedText);
    }
}
```

Вывод:
```
До: The quick brown fox jumps over the lazy dog
После: The quick brown fox jumps over the energetic dog
```

Теперь, для шаблонов или более сложных замен, в игру вступают `Pattern` и `Matcher`:

```java
import java.util.regex.Pattern;
import java.util.regex.Matcher;

public class RegexReplaceDemo {
    public static void main(String[] args) {
        String originalText = "There are 31,536,000 seconds in 365 days.";
        Pattern pattern = Pattern.compile("\\d+");
        Matcher matcher = pattern.matcher(originalText);
        String modifiedText = matcher.replaceAll("#");
        
        System.out.println("До: " + originalText);
        System.out.println("После: " + modifiedText);        
    }
}
```

Вывод:
```
До: There are 31,536,000 seconds in 365 days.
После: There are # seconds in # days.
```

## Подробнее:

Метод `replace()` восходит к самым ранним дням Java. Он является частью неизменяемого класса `String`, что означает, что каждый раз, используя его, вы создаете новую строку. Очень экологично, без отходов старого материала.

Но что насчет `Pattern` и `Matcher`, спросите вы? Эти классы являются частью API регулярных выражений (regex) в Java, представленных в Java 1.4. Они добавляют мощности поиску и замене, позволяя обнаруживать сложные шаблоны и динамически изменять текст. Это как использование скальпеля вместо кувалды.

К тому же, есть `replaceAll()` и `replaceFirst()`, два метода класса `Matcher`, которые точно настраивают ваши текстовые преобразования, заменяя все вхождения или только первое совпадение.

Еще одна альтернатива - использование классов `StringBuffer` или `StringBuilder`, когда вы имеете дело с тонной модификаций, потому что в отличие от `String`, эти буферы являются изменяемыми.

## Смотрите также:

- [Документация Java String](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- [Документация Java Pattern](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html)
- [Документация Matcher](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Matcher.html)
- [Учебник по регулярным выражениям](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html)

Для более практической практики посетите RegexOne (https://regexone.com), это отличный ресурс для повышения ваших навыков работы с regex.
