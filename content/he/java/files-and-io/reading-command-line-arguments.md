---
title:                "קריאת פרמטרים משורת הפקודה"
aliases:
- /he/java/reading-command-line-arguments.md
date:                  2024-01-20T17:56:55.117872-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת פרמטרים משורת הפקודה"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת ארגומנטים משורת הפקודה היא דרך להעניק קלט לתוכנה שלך דרך הטרמינל. תכניתי JAVA משתמשים בזה כדי להוסיף גמישות ולאפשר למשתמשים להשפיע על הרצת התוכנית מבלי לשנות את הקוד.

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
