---
date: 2024-01-20 17:47:57.335658-07:00
description: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05E9\u05DC\
  \ \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D6\u05D4 \u05DC\u05DE\u05E2\u05E9\u05D4\
  \ \u05DC\u05E1\u05E4\u05D5\u05E8 \u05DB\u05DE\u05D4 \u05EA\u05D5\u05D5\u05D9\u05DD\
  \ \u05D9\u05E9 \u05D1\u05D4. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\
  \u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D1\u05D3\
  \u05D5\u05E7 \u05EA\u05E7\u05D9\u05E0\u05D5\u05EA, \u05DC\u05E2\u05D1\u05D3 \u05D8\
  \u05E7\u05E1\u05D8 \u05D5\u05DC\u05D0\u05E8\u05D2\u05DF \u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD."
lastmod: '2024-03-13T22:44:39.115520-06:00'
model: gpt-4-1106-preview
summary: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05E9\u05DC \u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D6\u05D4 \u05DC\u05DE\u05E2\u05E9\u05D4 \u05DC\
  \u05E1\u05E4\u05D5\u05E8 \u05DB\u05DE\u05D4 \u05EA\u05D5\u05D5\u05D9\u05DD \u05D9\
  \u05E9 \u05D1\u05D4."
title: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA"
weight: 7
---

## איך עושים את זה:
```java
public class StringLengthExample {
    public static void main(String[] args) {
        String hebrewText = "שלום עולם";
        int length = hebrewText.length();

        System.out.println("The length of the string is: " + length);
    }
}
```
פלט לדוגמא:
```
The length of the string is: 9
```

## צלילה עמוקה
ב-Java, המתודה `.length()` היא הדרך הסטנדרטית למצוא את אורך המחרוזת מאז שהשפה נוצרה. שימו לב שהיא מחזירה את מספר התווים `char`, שהם לא בהכרח פינות תקשורת (code points) ביוניקוד, מה שיכול לגרום לסלעים במחרוזות רב לשוניות.

לחלופין, יש גם את `.codePointCount()` שמחשבת את מספר הפינות תקשורת. ההבדל הוא חשוב במיוחד בעבודה עם שפות כמו העברית, שבה יש תווים משולבים.

בפנים, `.length()` עוברת על מערך התווים שמאחסן את המחרוזת וסופרת אותם.

## ראו גם
- [מחרוזות ב-Java](https://docs.oracle.com/javase/10/docs/api/java/lang/String.html)
- [מחלקת Character](https://docs.oracle.com/javase/10/docs/api/java/lang/Character.html)
- [יוניקוד וJava](https://docs.oracle.com/javase/tutorial/i18n/text/unicode.html)
