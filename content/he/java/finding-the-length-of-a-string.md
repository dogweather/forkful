---
title:                "מציאת אורך מחרוזת"
date:                  2024-01-20T17:47:57.335658-07:00
model:                 gpt-4-1106-preview
simple_title:         "מציאת אורך מחרוזת"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
מציאת אורך של מחרוזת זה למעשה לספור כמה תווים יש בה. תכניתנים עושים זאת כדי לבדוק תקינות, לעבד טקסט ולארגן נתונים.

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
