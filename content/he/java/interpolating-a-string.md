---
title:                "שרבוב מחרוזת"
date:                  2024-01-20T17:51:15.742422-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרבוב מחרוזת"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/interpolating-a-string.md"
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