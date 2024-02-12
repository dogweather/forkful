---
title:                "מחיקת תווים התואמים לתבנית"
date:                  2024-01-20T17:43:00.170821-07:00
model:                 gpt-4-1106-preview
simple_title:         "מחיקת תווים התואמים לתבנית"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?

מחיקת תווים התואמים לדפוס מדברת על הסרת קבוצת תווים מתוך מחרוזת על פי קריטריון מסוים. תוכניתנים עושים זאת לטהר מחרוזת, להסיר תווים לא רצויים או לעבד טקסט בהתאם לדרישות מסוימות.

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