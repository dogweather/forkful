---
title:                "Поиск и замена текста"
aliases:
- /ru/java/searching-and-replacing-text.md
date:                  2024-01-29T00:02:30.393661-07:00
model:                 gpt-4-0125-preview
simple_title:         "Поиск и замена текста"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/java/searching-and-replacing-text.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
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
