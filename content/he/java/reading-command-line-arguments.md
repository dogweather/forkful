---
title:                "קריאה של ארגומנטים משורת הפקודה"
html_title:           "C#: קריאה של ארגומנטים משורת הפקודה"
simple_title:         "קריאה של ארגומנטים משורת הפקודה"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה ולמה?

הקריאה של ארגומנטים משורת הפקודה היא התהליך שבו מבצעים את הקוד בזמן שמעבירים פרמטרים אליו. זה מאפשר למתכנתים להתאים את התוכנית לגמרי, מאפשרת גמישות בזמן הריצה.

## כיצד לעשות:

הנה דוגמא לקוד שבו התוכנית מקבלת ארגומנטים משורת הפקודה:

```Java
public class Main {
    public static void main(String[] args) {
        for (String arg : args) {
            System.out.println("ארגומנט משורת הפקודה : " + arg);
        }
    }
}
```
**דוגמה לפלט של התוכנית:**

ארגומנט משורת הפקודה: תורמלינים
ארגומנט משורת הפקודה: הם
ארגומנט משורת הפקודה: אבנים
ארגומנט משורת הפקודה: מדהימות

## צלילה עמוקה:

אמנם לקרוא לארגומנטים משורת הפקודה הוא נושא מוכר למרבית מתכנתי Java, המכניזם הזה הוא מרכזי לכמה שפות תכנות אחרות כגון C, Python, ו-Shell script. גם ב-Java, יש גרסאות נוספות לקריאה של ארגומנטים, כמו באמצעות המחלקה `Scanner` או `Console`.

## ראה גם:

- דוקומנטציה של Oracle: 
[Command-Line Arguments](https://docs.oracle.com/en/java/javase/14/docs/specs/sealed-classes-jls.html#jls-13.1)
- חומרי למידה מתוך GeeksforGeeks: 
[Command line Arguments in Java](https://www.geeksforgeeks.org/command-line-arguments-in-java/)