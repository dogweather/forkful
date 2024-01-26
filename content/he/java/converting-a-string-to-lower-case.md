---
title:                "המרת מחרוזת לאותיות קטנות"
date:                  2024-01-20T17:38:55.685368-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרה של מחרוזת לאותיות קטנות זה תהליך שבו כל האותיות הגדולות במחרוזת מוחלפות באותיות קטנות. תכנתים עושים את זה לצורך אחידות, השוואת מחרוזות בלי תלות ברישיות, וכדי לטפל בקלט של משתמש באופן מנומס.

## איך לעשות:
```java
public class LowerCaseExample {
    public static void main(String[] args) {
        String originalString = "Hello, World!";
        String lowerCaseString = originalString.toLowerCase();
        System.out.println(lowerCaseString); // פלט: hello, world!
    }
}
```
פשוט קוראים למתודה `toLowerCase()` על המחרוזת שרוצים להמיר.

## עיון מעמיק
המרות רישיות במחרוזות היו חלק משפות תכנות מהתחלותיהן, אבל הדרך שבה זה נעשה תלויה בתרבות ובשפה. ב-Java, המתודה `toLowerCase()` נעזרת בספריית ה-Unicode כדי לתמוך במגוון שפות. ישנן גם חלופות: ניתן להשתמש ב`Locale` כדי לקבוע את ההתנהגות הספציפית לתרבות מסוימת, כמו בדוגמה הבאה:

```java
String withLocale = originalString.toLowerCase(new Locale("tr", "TR")); // טורקית
```

הבדלים בין תרבויות יכולים להשפיע, למשל בטורקית ישנה אות 'I' בגרסה קטנה שונה מהאנגלית. חשוב לדעת את ההבדלים האלה כשעובדים עם טקסט בינלאומי.

## ראו גם:
- מדריך Oracle למחרוזת `toLowerCase()`: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toLowerCase()
- תיעוד המחלקה `Locale` ב-Java: https://docs.oracle.com/javase/7/docs/api/java/util/Locale.html
- הסבר על Unicode ו-Java: https://www.oracle.com/technical-resources/articles/javase/supplementary.html

קראו את המקורות להעשרת הידע ולטיפים נוספים על עבודה עם טקסט ב-Java.
