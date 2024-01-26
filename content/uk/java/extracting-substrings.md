---
title:                "Виділення підрядків"
date:                  2024-01-20T17:45:59.607635-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виділення підрядків"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?

Витягування підрядків у Java - це спосіб отримання частини з рядка. Програмісти роблять це, щоб обробити або використовувати конкретні дані з більшого тексту.

## Як це зробити:

```java
public class SubstringExample {
    public static void main(String[] args) {
        String fullString = "Привіт, світе!";
        String partOfString = fullString.substring(8, 13); // Витягує "світе"
        
        System.out.println("Повний рядок: " + fullString);
        System.out.println("Підрядок: " + partOfString);
    }
}

/*
Вивід:
Повний рядок: Привіт, світе!
Підрядок: світе
*/
```
Використовуйте метод `.substring()` для отримання частини рядка. Передайте індекс початку і кінця, щоб обмежити витягування.

## Поглиблений огляд:

В Java витягнення підрядків стало зазвичай завдяки `String` класу. З часів Java 1, ця можливість допомагає оптимізувати обробку текстів. Останні версії Java управляють рядками ефективніше, що зменшує пам'ять та прискорює доступ.

Альтернативами є методи `split()` або регулярні вирази (`Pattern` і `Matcher` класи), кожен із своїм використанням. Застосування `substring()` краще для простого і точного витягування певних сегментів тексту.

Реалізаційно, до Java 7, `substring()` створював новий рядок зі спільним масивом символів - економія пам'яті. Але з Java 7, `substring()` створює новий масив символів для кожного нового рядка, ліквідуючи залежність від оригінального рядка та потенційні витоки пам'яті.

## Дивіться також:

- [String (Java Platform SE 8)](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Class Pattern (Java Platform SE 8)](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
- [Class Matcher (Java Platform SE 8)](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Matcher.html)
- [Effective Java (Third Edition) by Joshua Bloch](https://www.pearson.com/us/higher-education/program/Bloch-Effective-Java-3rd-Edition/PGM334830.html) - розділ про керування рядками для глибшого розуміння.
