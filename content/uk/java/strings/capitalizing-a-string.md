---
title:                "Зробити першу літеру рядка великою"
aliases: - /uk/java/capitalizing-a-string.md
date:                  2024-02-03T19:06:10.456224-07:00
model:                 gpt-4-0125-preview
simple_title:         "Зробити першу літеру рядка великою"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та чому?
Перетворення рядка з великої літери включає зміну першої літери кожного слова в рядку на велику, одночасно забезпечуючи, щоб решта букв залишилися маленькими. Це поширене завдання з маніпулювання рядками корисне для форматування тексту в додатках, наприклад, підготовки користувацьких імен або заголовків для відображення відповідно до конвенцій чи граматичної правильності.

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
