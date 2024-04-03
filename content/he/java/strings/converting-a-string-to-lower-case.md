---
date: 2024-01-20 17:38:55.685368-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: ."
lastmod: '2024-03-13T22:44:39.109265-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA"
weight: 4
---

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
