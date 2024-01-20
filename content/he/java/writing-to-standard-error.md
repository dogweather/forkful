---
title:                "כתיבה לפלט השגיאה הסטנדרטי"
html_title:           "Arduino: כתיבה לפלט השגיאה הסטנדרטי"
simple_title:         "כתיבה לפלט השגיאה הסטנדרטי"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה ל-standard error היא אמצעי להפצת הודעות שגיאה או לוגים קריטיים. תכניתנים עושים זאת כדי להפריד בין הפלט הרגיל לבין הודעות שגיאה, ולאפשר ניהול וגישה נוחה יותר.

## איך לעשות:
```java
public class StdErrExample {
    public static void main(String[] args) {
        System.out.println("זוהי הודעה לפלט רגיל (stdout).");
        System.err.println("זוהי הודעת שגיאה (stderr).");
    }
}
```
פלט:
```
זוהי הודעה לפלט רגיל (stdout).
זוהי הודעת שגיאה (stderr).
```

## עיון נוסף:
בעבר, stdout וstderr היו מאוד חשובים בתכנות Unix, שם הפלט הייתה נפרד במפורש משגיאות. אלטרנטיבות יכולות להיות כתיבה לקובץ לוג משלך או שימוש בספריות לוגינג כמו Log4j. ב-Java, `System.err` היא ערוץ פלט סטנדרטי שמייצג את stderr, והיא מופעלת דרך פעולת זרימת פלט (`PrintStream`).

## ראה גם:
- [Java API Documentation for System.err](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#err)
- [Oracle tutorial on I/O streams in Java](https://docs.oracle.com/javase/tutorial/essential/io/streams.html)
- [Log4j – Logging Framework](https://logging.apache.org/log4j/2.x/)