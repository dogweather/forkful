---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:26.874980-07:00
description: "\u042F\u043A: \u0412\u0431\u0443\u0434\u043E\u0432\u0430\u043D\u0430\
  \ \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u043A\u0430 regex \u0432 Java \u0437\
  \u0434\u0456\u0439\u0441\u043D\u044E\u0454\u0442\u044C\u0441\u044F \u0433\u043E\u043B\
  \u043E\u0432\u043D\u0438\u043C \u0447\u0438\u043D\u043E\u043C \u0447\u0435\u0440\
  \u0435\u0437 \u043A\u043B\u0430\u0441\u0438 `Pattern` \u0442\u0430 `Matcher` \u0443\
  \ \u043F\u0430\u043A\u0435\u0442\u0456 `java.util.regex`. \u041E\u0441\u044C \u043F\
  \u0440\u043E\u0441\u0442\u0438\u0439 \u043F\u0440\u0438\u043A\u043B\u0430\u0434\
  , \u0449\u043E\u0431\u2026"
lastmod: '2024-03-13T22:44:49.064100-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u0431\u0443\u0434\u043E\u0432\u0430\u043D\u0430 \u043F\u0456\u0434\
  \u0442\u0440\u0438\u043C\u043A\u0430 regex \u0432 Java \u0437\u0434\u0456\u0439\u0441\
  \u043D\u044E\u0454\u0442\u044C\u0441\u044F \u0433\u043E\u043B\u043E\u0432\u043D\u0438\
  \u043C \u0447\u0438\u043D\u043E\u043C \u0447\u0435\u0440\u0435\u0437 \u043A\u043B\
  \u0430\u0441\u0438 `Pattern` \u0442\u0430 `Matcher` \u0443 \u043F\u0430\u043A\u0435\
  \u0442\u0456 `java.util.regex`."
title: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0440\
  \u0435\u0433\u0443\u043B\u044F\u0440\u043D\u0438\u0445 \u0432\u0438\u0440\u0430\u0437\
  \u0456\u0432"
weight: 11
---

## Як:
Вбудована підтримка regex в Java здійснюється головним чином через класи `Pattern` та `Matcher` у пакеті `java.util.regex`. Ось простий приклад, щоб знайти та вивести всі входження слова у рядку, не звертаючи уваги на регістр:

```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexExample {
    public static void main(String[] args) {
        String text = "Regex is great for parsing. Parsing with regex is powerful.";
        String wordToFind = "parsing";
        
        Pattern pattern = Pattern.compile(wordToFind, Pattern.CASE_INSENSITIVE);
        Matcher matcher = pattern.matcher(text);
        
        while (matcher.find()) {
            System.out.println("Found '" + matcher.group() + "' at position " + matcher.start());
        }
    }
}
```

Вивід:
```
Знайдено 'parsing' на позиції 16
Знайдено 'Parsing' на позиції 31
```

Для завдань, таких як розбиття рядків, ви можете використовувати метод `split()` класу `String` з regex:

```java
public class SplitExample {
    public static void main(String[] args) {
        String text = "Java,Python,Ruby,JavaScript";
        String[] languages = text.split(",");
        
        for (String language : languages) {
            System.out.println(language);
        }
    }
}
```

Вивід:
```
Java
Python
Ruby
JavaScript
```

Працюючи з regex в Java, можуть бути випадки, коли зовнішня бібліотека може спростити складні завдання. Одна з популярних сторонніх бібліотек для роботи з regex в Java - це `Apache Commons Lang`. Вона пропонує утиліти, такі як `StringUtils`, які роблять деякі завдання regex більш простими. Ось як використовувати її для підрахунку збігів підрядка:

```java
import org.apache.commons.lang3.StringUtils;

public class CommonsLangExample {
    public static void main(String[] args) {
        String text = "Regex makes text processing easier. Processing text with regex is efficient.";
        String substring = "processing";
        
        int count = StringUtils.countMatches(text, substring);
        System.out.println("'" + substring + "' зустрічається " + count + " разів.");
    }
}
```

Щоб використовувати Apache Commons Lang, вам потрібно включити її в ваш проект. Якщо ви використовуєте Maven, додайте цю залежність до вашого `pom.xml`:

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-lang3</artifactId>
    <version>3.12.0</version> <!-- Перевірте на наявність останньої версії -->
</dependency>
```

Вивід:
```
'processing' зустрічається 2 рази.
```
