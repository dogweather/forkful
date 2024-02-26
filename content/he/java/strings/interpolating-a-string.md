---
date: 2024-01-20 17:51:15.742422-07:00
description: "\u05D0\u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\
  \u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D4\u05D9\u05D0 \u05D8\u05DB\u05E0\
  \u05D9\u05E7\u05D4 \u05DC\u05E9\u05DC\u05D1 \u05DE\u05E9\u05EA\u05E0\u05D9\u05DD\
  \ \u05D5\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05D1\u05EA\u05D5\u05DA \u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05DE\u05D1\u05DC\u05D9 \u05DC\u05D4\u05E9\u05EA\
  \u05DE\u05E9 \u05D1\u05E7\u05D8\u05D9\u05E0\u05D2 \u05D5\u05D4\u05D3\u05D1\u05E7\
  \u05D4 \u05DE\u05E1\u05D5\u05E8\u05D1\u05DC\u05D9\u05DD. \u05D6\u05D4 \u05D7\u05D5\
  \u05E1\u05DA \u05D6\u05DE\u05DF \u05E4\u05D9\u05EA\u05D5\u05D7 \u05D5\u05DE\u05E7\
  \u05DC \u05E2\u05DC \u05E7\u05E8\u05D9\u05D0\u05D5\u05EA \u05D5\u05EA\u05D7\u05D6\
  \u05D5\u05E7\u05EA \u05D4\u05E7\u05D5\u05D3."
lastmod: '2024-02-25T18:49:37.344682-07:00'
model: gpt-4-1106-preview
summary: "\u05D0\u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\
  \u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D4\u05D9\u05D0 \u05D8\u05DB\u05E0\
  \u05D9\u05E7\u05D4 \u05DC\u05E9\u05DC\u05D1 \u05DE\u05E9\u05EA\u05E0\u05D9\u05DD\
  \ \u05D5\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05D1\u05EA\u05D5\u05DA \u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05DE\u05D1\u05DC\u05D9 \u05DC\u05D4\u05E9\u05EA\
  \u05DE\u05E9 \u05D1\u05E7\u05D8\u05D9\u05E0\u05D2 \u05D5\u05D4\u05D3\u05D1\u05E7\
  \u05D4 \u05DE\u05E1\u05D5\u05E8\u05D1\u05DC\u05D9\u05DD. \u05D6\u05D4 \u05D7\u05D5\
  \u05E1\u05DA \u05D6\u05DE\u05DF \u05E4\u05D9\u05EA\u05D5\u05D7 \u05D5\u05DE\u05E7\
  \u05DC \u05E2\u05DC \u05E7\u05E8\u05D9\u05D0\u05D5\u05EA \u05D5\u05EA\u05D7\u05D6\
  \u05D5\u05E7\u05EA \u05D4\u05E7\u05D5\u05D3."
title: "\u05E9\u05E8\u05D1\u05D5\u05D1 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
אינטרפולציה של מחרוזת היא טכניקה לשלב משתנים וביטויים בתוך מחרוזת מבלי להשתמש בקטינג והדבקה מסורבלים. זה חוסך זמן פיתוח ומקל על קריאות ותחזוקת הקוד.

## איך לעשות:
```java
public class StringInterpolationExample {
    public static void main(String[] args) {
        String name = "דוד";
        int age = 30;
        String message = String.format("שלום, אני %s ואני בן %d", name, age);
        System.out.println(message);  // פלט: שלום, אני דוד ואני בן 30
    }
}
```

## עיון נוסף:
ב-Java, אינטרפולציה של מחרוזת אינה ישירה כמו בשפות אחרות - אין לנו סינטקס ייחודי. במקום זאת, אנחנו משתמשים ב`String.format()`. בעבר השתמשו בשרשור מחרוזות, דבר שיכול להוביל לקוד מבולגן. כיום, ישנם ספריות חיצוניות כמו Apache Commons Lang שמציעות גרסאות שונות ומתקדמות של אינטרפולציה.

## ראה גם:
- דוקומנטציה של `String.format()`: https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html#format(java.lang.String,java.lang.Object...)
- Apache Commons Lang `StringUtils`: https://commons.apache.org/proper/commons-lang/javadocs/api-release/org/apache/commons/lang3/StringUtils.html
- גישות אלטרנטיביות לאינטרפולציה ב-Java: https://www.baeldung.com/java-string-formatting
