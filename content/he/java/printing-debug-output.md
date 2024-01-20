---
title:                "הדפסת פלט ניפוי שגיאות"
html_title:           "Arduino: הדפסת פלט ניפוי שגיאות"
simple_title:         "הדפסת פלט ניפוי שגיאות"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה ולמה?
הדפסת התראות לניפוי שגיאות (Debug Output) היא שיטה שמאפשרת למתכנתים לראות את הזרימה של התוכנית שלהם תוך כדי ריצה או לבחינת משתנים. מתכנתים משתמשים בזה כדי לנפות שגיאות ולקבוע את מקום הבעיה בקוד.

## שימוש:
בממשק `System.out` יש מספר שיטות שאנחנו יכולים להשתמש בהם: `print()`, `println()`, `printf()`. על ידי השתמש בהם, אנחנו יכולים להדפיס הודעות לניפוי שגיאות.

```Java
public class Main {
    public static void main(String[] args) {
        System.out.println("This is a debug message");
        
        int x = 10;
        System.out.println("The value of x is: " + x);
    }
}
```

תוצאה:

```
This is a debug message
The value of x is: 10
```

## צלילה עמוקה:
הדפסת הודעות לניפוי שגיאות נמצאת בשימוש מפוארב בימינו, אך זו לא השיטה היחידה לניפוי שגיאות. אפשר להשתמש גם בדפיסת ההילה (tracing), במעקב-נקודה (breakpoints), במתנדים (watchpoints) ועוד. זה בהחלט עניין של טעם אישי ומסיבות איכות. אם ההודעה לא מסייעת באיתור הבעיה או שהיא מכבידה על הביצועים של המערכת, ייתכן ותרצה לשקול שיטות אחרות.

## ראה גם:
1. [Oracle Java Documentation](https://docs.oracle.com/en/java/)
2. [Oracle’s Java Tutorials – Basic I/O](https://docs.oracle.com/javase/tutorial/essential/io/index.html)
3. [Introduction to Java Debugging](http://www.cs.cf.ac.uk/Dave/PERL/node157.html)