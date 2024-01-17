---
title:                "חיבור מחרוזות"
html_title:           "Java: חיבור מחרוזות"
simple_title:         "חיבור מחרוזות"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/concatenating-strings.md"
---

{{< edit_this_page >}}

מה ולמה?
לחיבור מחרוזות הוא תהליך המאפשר למתכנתים לשלב מחרוזות שונות יחד וליצור מחרוזת חדשה. פתרון זה מאפשר לנו ליצור טכסט מיוצג ותוכן משולב ממספר מקורות שונים.

איך לעשות זאת?
להלן, דוגמאות של קוד ופלט לחיבור מחרוזות בשפת Java:

```java
// דוגמה ראשונה: חיבור של שני מחרוזות
String message = "שלום";
String name = "עמית";
String greeting = message + " " + name + "!";
System.out.println(greeting); // הפלט המצורף יהיה: שלום עמית!

// דוגמה שנייה: חיבור של מספרים למחרוזת
int age = 30;
String birthdayMessage = "יש לי " + age + " שנים! אחלה גיל, הניח שתוא סטאר.";
System.out.println(birthdayMessage); // הפלט המצורף יהיה: יש לי 30 שנים! אחלה גיל, הניח שתוא סטאר.
```

טיפ קטן: יש לזכור שבשפת Java, יש להשתמש בסימן הפלוס (+) לחיבור מחרוזות.

לכל הקשר
בהיסטוריה של שפות התכנות, קיוו התכנתים לאחד או להקטין את מספר הפעולות כדי להפוך את חיבור מחרוזות לפעולה יותר פשוטה ויעילה. בשפות רבות, חיבור מחרוזות מצריך הפרדת המחרוזות באמצעות פסיקים או סימנים אחרים, מה שעשוי להיות מסורבל לתכנתן. עם כניסת שפת Java למדפסת, חיבור מחרוזות הפך להיות פעולה פשוטה בהרבה שאינה דורשת הפרדה בין המחרוזות.

ראו גם
למידע נוסף על חיבור מחרוזות בשפת Java, אנחנו ממליצים לראות את המקורות הבאים:

- תיעוד המפתחים של Java מכיל דגמאות ונתוני עניין נוספים על חיבור מחרוזות: https://docs.oracle.com/javase/tutorial/java/data/strings.html
- המדריך הרשמי של Java מתאר כיצד להשתמש במחרוזות בקושיות: https://docs.oracle.com/javase/tutorial/java/

conceptual/java/index.html
- באתר "Java Code Geeks" יש הסבר מפורט יותר על השתמשות בחיבור מחרוזות בשפת Java: https://www.javacodegeeks.com/2015/09/the
-string-concatenation-operator-in-java.html