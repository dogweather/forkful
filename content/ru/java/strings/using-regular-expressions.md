---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:27.654415-07:00
description: "\u041A\u0430\u043A \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\
  \u0430\u0442\u044C: \u0427\u0442\u043E\u0431\u044B \u0438\u0441\u043F\u043E\u043B\
  \u044C\u0437\u043E\u0432\u0430\u0442\u044C regex \u0432 Java, \u0432\u0430\u043C\
  \ \u043D\u0443\u0436\u043D\u044B \u043A\u043B\u0430\u0441\u0441\u044B `Pattern`\
  \ \u0438 `Matcher` \u0438\u0437 `java.util.regex`. \u0412\u043E\u0442 \u043F\u0440\
  \u0438\u043C\u0435\u0440 \u043F\u043E\u0438\u0441\u043A\u0430 \u0430\u0434\u0440\
  \u0435\u0441\u043E\u0432 \u044D\u043B\u0435\u043A\u0442\u0440\u043E\u043D\u043D\u043E\
  \u0439\u2026"
lastmod: '2024-03-13T22:44:44.801098-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0442\u043E\u0431\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\
  \u043E\u0432\u0430\u0442\u044C regex \u0432 Java, \u0432\u0430\u043C \u043D\u0443\
  \u0436\u043D\u044B \u043A\u043B\u0430\u0441\u0441\u044B `Pattern` \u0438 `Matcher`\
  \ \u0438\u0437 `java.util.regex`."
title: "\u0418\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\
  \ \u0440\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u044B\u0445 \u0432\u044B\u0440\
  \u0430\u0436\u0435\u043D\u0438\u0439"
weight: 11
---

## Как использовать:
Чтобы использовать regex в Java, вам нужны классы `Pattern` и `Matcher` из `java.util.regex`. Вот пример поиска адресов электронной почты в строке.

```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexExample {
    public static void main(String[] args) {
        String text = "Contact me at hello@world.com or buzz@space.net.";
        String emailRegex = "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}\\b";

        Pattern pattern = Pattern.compile(emailRegex);
        Matcher matcher = pattern.matcher(text);

        while (matcher.find()) {
            System.out.println(matcher.group());
        }
    }
}
```
Вывод:
```
hello@world.com
buzz@space.net
```

## Глубже в Тему
Регулярные выражения существуют с 1950-х годов, их изобрел математик Стивен Клини. В Java регулярные выражения интегрированы начиная с версии 1.4. Несмотря на их мощь, regex может быть избыточным для простых операций со строками — методы вроде `String.contains()`, `String.split()`, и `String.startsWith()` являются простыми альтернативами для базовых сценариев. Под капотом, движок регулярных выражений Java (используя `Pattern` и `Matcher`) компилирует шаблон в серию инструкций байт-кода, которые исполняются `Matcher` против входной строки.

## Смотрите Также
Узнайте больше о regex в Java с этими ресурсами:
- [Java Класс Pattern](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html)
- [Java Класс Matcher](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Matcher.html)
- [Oracle Java Учебник: Регулярные Выражения](https://docs.oracle.com/javase/tutorial/essential/regex/)
- [Regular-Expressions.info для глубокого погружения в синтаксис и шаблоны регулярных выражений](https://www.regular-expressions.info/)
