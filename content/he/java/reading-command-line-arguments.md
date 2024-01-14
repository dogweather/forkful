---
title:                "Java: קריאת ארגומנטים בפקודת השורה"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## למה

ניתן להשתמש במשתני שורת פקודה כדי לספק קלט נתונים לתוכניות ג'אווה ולשנות את הפעולה שלהן בהתאם. קריאת פרמטרים משורת הפקודה יכולה לשמש ליישום גישה אינטראקטיבית לתוכניות ג'אווה, בדיקת תקינות של הקלט ועוד. כתיבת קוד שמאפשר ניתוח פרמטרים של שורת פקודה יכול לקלט קלט נתונים מגוונים ולהפוך את קוד התוכנית לגמיש יותר.

## איך לעשות זאת

כדי לקרוא פרמטרים משורת הפקודה בג'אווה, ניתן להשתמש במחלקת "ArgsParser" המוגדרת כך:

```Java
import java.util.Arrays;

public class ArgsParser {
    public static void main(String[] args) {
        System.out.println("הפרמטרים שנזנו הם: " + Arrays.toString(args));
    }
}
```

ניתן להריץ את הקוד הזה עם הפרמטרים הרצויים משורת הפקודה ולקבל כתוצאה את הפרמטרים המוזנים בצורה נוחה לאיחסון ושימוש.

## לחקור עוד

כדי לע深入了解如何读取命令行参数，可以参考Java官方文档[ArgsParser documentation](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html). ניתן גם לקרוא על זה במדריך ה- [CMD parametes in Java](https://www.baeldung.com/java-command-line-arguments) של בלגון. לשים לב שגירסת הג'אווה הניתנת לתמיכה משלבת כמה שינויים מבין המדריכים המומלצים, אך המושכים בכלי ArgsParser הם עובדים עם גירסאות כדוגמת Java 8 והמעלה.

## ראה גם

- [Java ספר התכנות](https://he.wikipedia.org/wiki/Java)
- [Java דוקומנטציה רשמית](https://docs.oracle.com/javase/8/docs/api/)