---
date: 2024-01-20 17:35:26.274044-07:00
description: "How to: (\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D0\u05EA\
  \ \u05D6\u05D4?) \u05E7\u05D5\u05D3 Java \u05E4\u05E9\u05D5\u05D8 \u05E9\u05DE\u05E8\
  \u05D0\u05D4 \u05D0\u05D9\u05DA \u05DC\u05E9\u05E8\u05E9\u05E8 \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05D5\u05EA."
lastmod: '2024-04-05T21:53:40.354683-06:00'
model: gpt-4-1106-preview
summary: "(\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D0\u05EA \u05D6\u05D4\
  ?) \u05E7\u05D5\u05D3 Java \u05E4\u05E9\u05D5\u05D8 \u05E9\u05DE\u05E8\u05D0\u05D4\
  \ \u05D0\u05D9\u05DA \u05DC\u05E9\u05E8\u05E9\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA."
title: "\u05E9\u05E8\u05E9\u05D5\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA"
weight: 3
---

## How to: (איך לעשות את זה?)
קוד Java פשוט שמראה איך לשרשר מחרוזות:

```java
public class StringConcatExample {
    public static void main(String[] args) {
        String hello = "שלום";
        String world = "עולם";
        String greeting = hello + " ל" + world + "!";

        System.out.println(greeting);
    }
}
```

פלט דוגמא:

```
שלום לעולם!
```

## Deep Dive (עמוק יותר)
בעבר, שרשור מחרוזות ישירות עלול היה להיות פחות יעיל כאשר משתמשים בלולאות גדולות כי כל שרשור יצר עותק חדש של המחרוזת. מאז Java 5, היתרון הזה נעלם כי קומפיילר ה-Java משתמש במחלקה StringBuilder כדי לייעל את התהליך.

לחלופין, ניתן להשתמש במחלקות כמו `StringBuilder` או `StringBuffer`:

```java
StringBuilder builder = new StringBuilder();
builder.append("שלום");
builder.append(" ל");
builder.append("עולם!");
String greeting = builder.toString();

System.out.println(greeting);
```

`StringBuffer` מתאים לקוד מרובה תהליכים כי הוא מסנכרן את השיטות, בעוד `StringBuilder` מהיר יותר אך לא בטיחות חוטית (thread-safe).

## See Also (ראה גם)
- [Oracle Java documentation on String](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html) - מידע רשמי מתיעוד אורקל בנושא מחרוזות ב-Java (קישור עלול להשתנות בהתאם לגירסה).
- [Oracle Java documentation on StringBuilder](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/StringBuilder.html) - מידע רשמי בנושא StringBuilder.
- [Effective Java by Joshua Bloch](https://www.pearson.com/us/higher-education/program/Bloch-Effective-Java/PGM308398.html) - ספר מומלץ לקבלת הבנה טובה יותר על תכנות נכון ויעיל ב-Java.
