---
title:                "רישום פעולות (לוגים)"
date:                  2024-01-26T01:06:52.424048-07:00
model:                 gpt-4-1106-preview
simple_title:         "רישום פעולות (לוגים)"
programming_language: "Java"
category:             "Java"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/logging.md"
---

{{< edit_this_page >}}

## מה ולמה?
לוגינג הוא למעשה התהליך של תיעוד אירועים המתרחשים בתוך אפליקציית תוכנה. מתכנתים מתעדים אירועים אלה כדי לתפוס מידע בזמן ריצה, לאבחן בעיות, לפקח על התנהגות המערכת, וליצור מסלול ביקורת עבור מטרות אבטחה והתאמה לתקנים.

## כיצד לעשות זאת:
הנה דרך פשוטה להתחיל עם לוגינג ב-Java באמצעות החבילה המובנית `java.util.logging`.

```java
import java.util.logging.Logger;
import java.util.logging.Level;

public class AppLogging {
    private final static Logger LOGGER = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

    public static void main(String[] args) {
        LOGGER.info("Logging an INFO-level message");

        try {
            int division = 10 / 0;
        } catch (ArithmeticException e) {
            LOGGER.log(Level.SEVERE, "אירעה חריגה", e);
        }
    }
}
```

זה יפיק פלט בסגנון הבא:

```
יול 03, 2023 2:00:00 PM AppLogging main
INFO: תיעוד הודעה ברמת INFO
יול 03, 2023 2:00:00 PM AppLogging main
SEVERE: אירעה חריגה
java.lang.ArithmeticException: / by zero
    at AppLogging.main(AppLogging.java:10)
```

## עיון מעמיק
לוגינג ב-Java התפתח לאורך השנים. בעבר, לוגינג היה דבר יותר חד-פעמי עם פלטים של המערכת ומנגנונים שנכתבו באופן עצמאי. למרות זאת, הצורך בתקנון הביא לידי היווצרות של ממשקי תכנות ללוגינג כמו `Log4j` ו-`SLF4J`. החבילה `java.util.logging` הוצגה ב-JDK 1.4, ומספקת דרך מתוקננת לתעד הודעות.

אלטרנטיבות ל-`java.util.logging` (JUL) כוללות את Log4j 2 ו-SLF4J. למרות ש-JUL מובנית ב-Java ולכן אינה דורשת תלויות נוספות, Log4j 2 ו-SLF4J מציעים תכונות מתקדמות יותר כמו שליטה דקה יותר על תצורת הלוגינג, לוגינג אסינכרוני, וביצועים טובים יותר.

מבחינת היישום, לוגינג יכול להיות סינכרוני, שבו כל הודעת לוג מעובדת באשך שיצר אותה, או אסינכרוני, שבו ההודעות מועברות לאשך נפרד. לוגינג אסינכרוני יכול לשפר ביצועים אך מצריך התמודדות עם סיבוכיות של ריבוי משימות והבטחה שהודעות לוג לא יאבדו במקרה של קריסת האפליקציה.

## ראה גם
- [Log4j 2](https://logging.apache.org/log4j/2.x/)
- [SLF4J](http://www.slf4j.org/)
- [סקירה רשמית של אורקל על לוגינג](https://docs.oracle.com/javase/8/docs/technotes/guides/logging/overview.html)
- [מדריך על java.util.logging](https://www.vogella.com/tutorials/Logging/article.html)