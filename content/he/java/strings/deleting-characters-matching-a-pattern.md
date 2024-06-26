---
date: 2024-01-20 17:43:00.170821-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05E8\u05D0\u05E9\
  \u05D9\u05EA, \u05D0\u05E0\u05D7\u05E0\u05D5 \u05E6\u05E8\u05D9\u05DB\u05D9\u05DD\
  \ \u05DC\u05D9\u05D9\u05D1\u05D0 \u05D0\u05EA \u05D4\u05D7\u05D1\u05D9\u05DC\u05D4\
  \ `java.util.regex` \u05E9\u05DE\u05DB\u05D9\u05DC\u05D4 \u05D0\u05EA \u05D4\u05DE\
  \u05D7\u05DC\u05E7\u05D5\u05EA \u05DC\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD\
  \ \u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05E8\u05D2\u05D5\u05DC\u05E8\u05D9\
  \u05D9\u05DD."
lastmod: '2024-03-13T22:44:39.104407-06:00'
model: gpt-4-1106-preview
summary: "\u05E8\u05D0\u05E9\u05D9\u05EA, \u05D0\u05E0\u05D7\u05E0\u05D5 \u05E6\u05E8\
  \u05D9\u05DB\u05D9\u05DD \u05DC\u05D9\u05D9\u05D1\u05D0 \u05D0\u05EA \u05D4\u05D7\
  \u05D1\u05D9\u05DC\u05D4 `java.util.regex` \u05E9\u05DE\u05DB\u05D9\u05DC\u05D4\
  \ \u05D0\u05EA \u05D4\u05DE\u05D7\u05DC\u05E7\u05D5\u05EA \u05DC\u05E2\u05D1\u05D5\
  \u05D3\u05D4 \u05E2\u05DD \u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05E8\u05D2\
  \u05D5\u05DC\u05E8\u05D9\u05D9\u05DD."
title: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\u05EA\
  \u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA"
weight: 5
---

## איך לעשות:
ראשית, אנחנו צריכים לייבא את החבילה `java.util.regex` שמכילה את המחלקות לעבודה עם ביטויים רגולריים.

```java
import java.util.regex.Pattern;
import java.util.regex.Matcher;

public class PatternMatcherExample {

    public static void main(String[] args) {
        String inputString = "אני לומד Java בשנת 2023!";
        String regex = "\\d"; // מבטא רגולרי למספרים

        // יצירת אובייקט ה-Pattern
        Pattern pattern = Pattern.compile(regex);

        // יצירת מחפש התאמות
        Matcher matcher = pattern.matcher(inputString);

        // כל זמן שמצאנו התאמה, נמחק אותה
        String output = matcher.replaceAll("");

        System.out.println("לפני: " + inputString);
        System.out.println("אחרי: " + output);
    }
}
```

פלט הדוגמה:
```
לפני: אני לומד Java בשנת 2023!
אחרי: אני לומד Java בשנת !
```

## צלילה לעומק
ביטויים רגולריים (Regex) הם כלי חזק ונפוץ לעיבוד מחרוזות. ההיסטוריה שלהם חוזרת לתחילת שנות ה-60. בעולם ה-Java, מחלקות כמו `Pattern` ו-`Matcher` מספקות את האמצעים ליישום ביטויים רגולריים. המחלקה `Pattern` מייצגת ביטוי רגולרי מקומפל, בעוד `Matcher` בודק התאמה של הביטוי הזה למחרוזת.

אם מדובר במחרוזות קטנות או פעולות פשוטות, ישנה אופציה להשתמש בשיטת `replaceAll` ישירות על מחרוזת. לעיתים, חלופות כמו `split` או `substring` יכולות גם הן לספק פתרון - תלוי בדרישות הספציפיות.

## ראה גם:
- [Java Pattern Class](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
- [Java Matcher Class](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Matcher.html)
- [RegexOne - למידת ביטויים רגולריים](https://regexone.com/)
- [Java Regular Expressions Tutorial](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)
