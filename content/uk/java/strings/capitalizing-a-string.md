---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:10.456224-07:00
description: "\u042F\u043A \u0437\u0440\u043E\u0431\u0438\u0442\u0438: \u0421\u0442\
  \u0430\u043D\u0434\u0430\u0440\u0442\u043D\u0430 \u0431\u0456\u0431\u043B\u0456\u043E\
  \u0442\u0435\u043A\u0430 Java \u043D\u0435 \u043D\u0430\u0434\u0430\u0454 \u043F\
  \u0440\u044F\u043C\u043E\u0433\u043E \u043C\u0435\u0442\u043E\u0434\u0443 \u0434\
  \u043B\u044F \u043F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F\
  \ \u0432\u0441\u0456\u0445 \u0440\u044F\u0434\u043A\u0456\u0432 \u043D\u0430 \u0432\
  \u0435\u043B\u0438\u043A\u0456 \u043B\u0456\u0442\u0435\u0440\u0438 \u043E\u0434\
  \u0440\u0430\u0437\u0443, \u0430\u043B\u0435 \u0432\u0438 \u043C\u043E\u0436\u0435\
  \u0442\u0435 \u0434\u043E\u0441\u044F\u0433\u0442\u0438 \u0446\u044C\u043E\u0433\
  \u043E \u0437\u0430\u2026"
lastmod: '2024-03-13T22:44:49.053303-06:00'
model: gpt-4-0125-preview
summary: "\u0421\u0442\u0430\u043D\u0434\u0430\u0440\u0442\u043D\u0430 \u0431\u0456\
  \u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0430 Java \u043D\u0435 \u043D\u0430\u0434\
  \u0430\u0454 \u043F\u0440\u044F\u043C\u043E\u0433\u043E \u043C\u0435\u0442\u043E\
  \u0434\u0443 \u0434\u043B\u044F \u043F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\
  \u0435\u043D\u043D\u044F \u0432\u0441\u0456\u0445 \u0440\u044F\u0434\u043A\u0456\
  \u0432 \u043D\u0430 \u0432\u0435\u043B\u0438\u043A\u0456 \u043B\u0456\u0442\u0435\
  \u0440\u0438 \u043E\u0434\u0440\u0430\u0437\u0443, \u0430\u043B\u0435 \u0432\u0438\
  \ \u043C\u043E\u0436\u0435\u0442\u0435 \u0434\u043E\u0441\u044F\u0433\u0442\u0438\
  \ \u0446\u044C\u043E\u0433\u043E \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\u043E\
  \u0433\u043E\u044E \u043A\u043E\u043C\u0431\u0456\u043D\u0430\u0446\u0456\u0457\
  \ \u0432\u0431\u0443\u0434\u043E\u0432\u0430\u043D\u0438\u0445 \u043C\u0435\u0442\
  \u043E\u0434\u0456\u0432."
title: "\u0417\u0440\u043E\u0431\u0438\u0442\u0438 \u043F\u0435\u0440\u0448\u0443\
  \ \u043B\u0456\u0442\u0435\u0440\u0443 \u0440\u044F\u0434\u043A\u0430 \u0432\u0435\
  \u043B\u0438\u043A\u043E\u044E"
weight: 2
---

## Як зробити:
Стандартна бібліотека Java не надає прямого методу для перетворення всіх рядків на великі літери одразу, але ви можете досягти цього за допомогою комбінації вбудованих методів. Для більш складних потреб сторонні бібліотеки, як-от Apache Commons Lang, пропонують прості рішення.

### Використання вбудованих методів Java
Щоб перетворити рядок на великі літери без зовнішніх бібліотек, ви можете розділити рядок на слова, зробити велику першу літеру кожного, а потім з'єднати їх назад. Ось простий підхід:

```java
public class CapitalizeString {
    public static void main(String[] args) {
        String text = "привіт, світ!";
        String capitalizedText = capitalizeWords(text);
        System.out.println(capitalizedText); // Виводить: "Привіт, Світ!"
    }

    public static String capitalizeWords(String str) {
        char[] chars = str.toLowerCase().toCharArray();
        boolean found = false;
        for (int i = 0; i < chars.length; i++) {
            if (!found && Character.isLetter(chars[i])) {
                chars[i] = Character.toUpperCase(chars[i]);
                found = true;
            } else if (Character.isWhitespace(chars[i]) || chars[i]=='.' || chars[i]=='\'') { 
                found = false;
            }
        }
        return String.valueOf(chars);
    }
}
```

Цей фрагмент коду перетворює весь рядок на маленькі літери, а потім ітерує через кожен символ, роблячи велику першу літеру кожного слова. Він вважає пробіли, точки й апострофи роздільниками слів.

### Використання Apache Commons Lang
Бібліотека Apache Commons Lang пропонує більш елегантне рішення за допомогою методу `WordUtils.capitalizeFully()`, який оброблює різні крайові випадки та роздільники за вас:

```java
// Додати залежність: org.apache.commons:commons-lang3:3.12.0

import org.apache.commons.text.WordUtils;

public class CapitalizeString {
    public static void main(String[] args) {
        String text = "привіт, світ!";
        String capitalizedText = WordUtils.capitalizeFully(text);
        System.out.println(capitalizedText); // Виводить: "Привіт, Світ!"
    }
}
```

Щоб використати цей метод, вам потрібно додати бібліотеку Apache Commons Lang до вашого проекту. Цей метод бібліотеки не лише робить великою першу літеру кожного слова, але й перетворює решту літер в кожному слові на маленькі, забезпечуючи послідовний паттерн капіталізації по всьому рядку.
