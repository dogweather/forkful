---
date: 2024-01-20 17:58:08.727548-07:00
description: "How to: (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438:) ."
lastmod: '2024-04-05T21:53:49.274976-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u041F\u043E\u0448\u0443\u043A \u0442\u0430 \u0437\u0430\u043C\u0456\u043D\
  \u0430 \u0442\u0435\u043A\u0441\u0442\u0443"
weight: 10
---

## How to: (Як це зробити:)
```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class TextSearchReplace {
    public static void main(String[] args) {
        String text = "Hello, this is a test. This test is simple.";
        String searchText = "test";
        String replaceText = "example";

        // Простий пошук і заміна
        String replacedText = text.replaceAll(searchText, replaceText);
        System.out.println(replacedText);  // Hello, this is an example. This example is simple.

        // Використання регулярних виразів
        Pattern pattern = Pattern.compile("(test)(\\s+)(is)", Pattern.CASE_INSENSITIVE);
        Matcher matcher = pattern.matcher(text);

        StringBuffer resultText = new StringBuffer();
        while (matcher.find()) {
            matcher.appendReplacement(resultText, "demo$2was");
        }
        matcher.appendTail(resultText);

        System.out.println(resultText.toString());  // Hello, this is a demo was simple. This demo was simple.
    }
}
```

## Deep Dive (Поглиблений Огляд):
Заміна тексту в Java була ускладнена до Java 5, коли введено клас `Matcher` та метод `appendReplacement`. Це дозволило працювати з регулярними виразами гнучкіше. Альтернативи `replaceAll` можуть включати працю над байтовими масивами або використання сторонніх бібліотек, як Apache Commons Lang. Ключове розуміння регулярних виразів відкриває можливість для складних маніпуляцій текстом. Але будьте обережні: складні регулярні вирази можуть бути важкі для розуміння та мають погану продуктивність.

## See Also (Дивіться також):
- [Java Pattern Class](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html)
- [Apache Commons Lang StringUtils](https://commons.apache.org/proper/commons-lang/javadocs/api-release/org/apache/commons/lang3/StringUtils.html)
- [Oracle Java Tutorials – Regular Expressions](https://docs.oracle.com/javase/tutorial/essential/regex/)
