---
date: 2024-01-20 17:56:55.117872-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D4\u05DE\u05E0\
  \u05D2\u05E0\u05D5\u05DF \u05E9\u05DC \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\
  \u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4\
  \ \u05D4\u05D5\u05D0 \u05D7\u05DC\u05E7 \u05DE\u05EA\u05DB\u05E0\u05D5\u05EA \u05DE\
  \u05D7\u05E9\u05D1\u05D9\u05DD \u05DE\u05D9\u05DE\u05D9 \u05D4\u05D3\u05D9\u05E0\
  \u05D5\u05D6\u05D0\u05D5\u05E8\u05D9\u05DD - \u05D6\u05D4 \u05E4\u05E9\u05D5\u05D8\
  \ \u05D5\u05D9\u05E9\u05D9\u05E8. \u05D9\u05E9 \u05D0\u05DC\u05D8\u05E8\u05E0\u05D8\
  \u05D9\u05D1\u05D5\u05EA \u05DB\u05DE\u05D5 \u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\
  \u05D5\u05D1\u05E5 \u05EA\u05E6\u05D5\u05E8\u05D4 \u05D0\u05D5 \u05E9\u05D9\u05DE\
  \u05D5\u05E9 \u05D1\u05DE\u05DE\u05E9\u05E7\u2026"
lastmod: '2024-04-05T22:50:53.371704-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05DE\u05E0\u05D2\u05E0\u05D5\u05DF \u05E9\u05DC \u05D0\u05E8\u05D2\
  \u05D5\u05DE\u05E0\u05D8\u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\
  \u05E7\u05D5\u05D3\u05D4 \u05D4\u05D5\u05D0 \u05D7\u05DC\u05E7 \u05DE\u05EA\u05DB\
  \u05E0\u05D5\u05EA \u05DE\u05D7\u05E9\u05D1\u05D9\u05DD \u05DE\u05D9\u05DE\u05D9\
  \ \u05D4\u05D3\u05D9\u05E0\u05D5\u05D6\u05D0\u05D5\u05E8\u05D9\u05DD - \u05D6\u05D4\
  \ \u05E4\u05E9\u05D5\u05D8 \u05D5\u05D9\u05E9\u05D9\u05E8."
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E4\u05E8\u05DE\u05D8\u05E8\u05D9\u05DD\
  \ \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4"
weight: 23
---

## איך לעשות:
```java
public class CommandLineExample {
    public static void main(String[] args) {
        // טיפול בארגומנטים הנשלחים
        if (args.length > 0) {
            System.out.println("הארגומנטים שהתקבלו:");
            for (String arg : args) {
                System.out.println(arg);
            }
        } else {
            System.out.println("לא התקבלו ארגומנטים.");
        }
    }
}
```
כדי להריץ:
```shell
java CommandLineExample פריט1 פריט2 פריט3
```

פלט דוגמא:
```
הארגומנטים שהתקבלו:
פריט1
פריט2
פריט3
```

## עומק הנושא
המנגנון של ארגומנטים משורת הפקודה הוא חלק מתכנות מחשבים מימי הדינוזאורים - זה פשוט וישיר. יש אלטרנטיבות כמו קריאת קובץ תצורה או שימוש בממשק משתמש גרפי, אבל לפעמים זה מוגזם. כשאתה משתמש ב-args[] ב-Java, אתה פשוט מקבל מערך של מחרוזות. תוכנה מתחילה לקרוא ארגומנטים מהאינדקס 0; אין גודל מרבי מוגדר, אבל תלוי במערכת ההפעלה והזיכרון.

## ראה גם:
- [Oracle Java Documentation](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Apache Commons CLI](https://commons.apache.org/proper/commons-cli/) - ספרייה לניתוח ארגומנטים משורת פקודה
- [JArgs](http://jargs.sourceforge.net/) - מנתח ארגומנטים משורת פקודה ב-Java
