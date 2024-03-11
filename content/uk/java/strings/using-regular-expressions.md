---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:26.874980-07:00
description: "\u0420\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u0456 \u0432\u0438\u0440\
  \u0430\u0437\u0438 (regex) \u0432 Java \u0434\u043E\u0437\u0432\u043E\u043B\u044F\
  \u044E\u0442\u044C \u0432\u0430\u043C \u0432\u0438\u0437\u043D\u0430\u0447\u0430\
  \u0442\u0438 \u0441\u043F\u0435\u0446\u0438\u0444\u0456\u0447\u043D\u0456 \u0448\
  \u0430\u0431\u043B\u043E\u043D\u0438 \u0434\u043B\u044F \u043F\u043E\u0448\u0443\
  \u043A\u0443, \u043C\u0430\u043D\u0456\u043F\u0443\u043B\u044E\u0432\u0430\u043D\
  \u043D\u044F \u0430\u0431\u043E \u0432\u0430\u043B\u0456\u0434\u0430\u0446\u0456\
  \u0457 \u0440\u044F\u0434\u043A\u0456\u0432 \u0443 \u0432\u0430\u0448\u043E\u043C\
  \u0443 \u043A\u043E\u0434\u0456. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\
  \u0441\u0442\u0438\u2026"
lastmod: '2024-03-11T00:14:22.911924-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u0456 \u0432\u0438\u0440\
  \u0430\u0437\u0438 (regex) \u0432 Java \u0434\u043E\u0437\u0432\u043E\u043B\u044F\
  \u044E\u0442\u044C \u0432\u0430\u043C \u0432\u0438\u0437\u043D\u0430\u0447\u0430\
  \u0442\u0438 \u0441\u043F\u0435\u0446\u0438\u0444\u0456\u0447\u043D\u0456 \u0448\
  \u0430\u0431\u043B\u043E\u043D\u0438 \u0434\u043B\u044F \u043F\u043E\u0448\u0443\
  \u043A\u0443, \u043C\u0430\u043D\u0456\u043F\u0443\u043B\u044E\u0432\u0430\u043D\
  \u043D\u044F \u0430\u0431\u043E \u0432\u0430\u043B\u0456\u0434\u0430\u0446\u0456\
  \u0457 \u0440\u044F\u0434\u043A\u0456\u0432 \u0443 \u0432\u0430\u0448\u043E\u043C\
  \u0443 \u043A\u043E\u0434\u0456. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\
  \u0441\u0442\u0438\u2026"
title: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0440\
  \u0435\u0433\u0443\u043B\u044F\u0440\u043D\u0438\u0445 \u0432\u0438\u0440\u0430\u0437\
  \u0456\u0432"
---

{{< edit_this_page >}}

## Що і чому?

Регулярні вирази (regex) в Java дозволяють вам визначати специфічні шаблони для пошуку, маніпулювання або валідації рядків у вашому коді. Програмісти використовують їх для завдань, як-от аналіз лог-файлів, валідація вводу користувача або пошук специфічних патернів у тексті, що дозволяє здійснювати складну обробку рядків з мінімальними зусиллями.

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
