---
title:                "Использование регулярных выражений"
date:                  2024-01-29T00:04:27.654415-07:00
model:                 gpt-4-0125-preview
simple_title:         "Использование регулярных выражений"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/java/using-regular-expressions.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?
Регулярные выражения (regex) — это шаблоны, используемые для поиска сочетаний символов в тексте. Программисты используют их для поиска, редактирования или манипулирования строками эффективно — экономя время и количество строк кода.

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
