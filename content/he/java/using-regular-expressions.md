---
title:                "שימוש בביטויים רגולריים"
date:                  2024-01-19
html_title:           "Bash: שימוש בביטויים רגולריים"
simple_title:         "שימוש בביטויים רגולריים"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## מה ולמה?
רגולר אקספרשנז (ביטויים רגילים) הם כלים לחיפוש ומניפולציה של מחרוזות בהתאם לתבניות מסוימות. תכניתנים משתמשים בהם כדי לחסוך זמן, לקודד בצורה יעילה יותר ולהבטיח נכונות וביטחון של הנתונים.

## איך לעשות:
הנה כמה דוגמאות בשפת ג'אווה:

```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexExamples {
    public static void main(String[] args) {
        // זיהוי תבנית מייל
        String emailRegex = "^(.+)@(.+)$";
        Pattern emailPattern = Pattern.compile(emailRegex);
        Matcher emailMatcher = emailPattern.matcher("example@domain.com");

        if (emailMatcher.find()) {
            System.out.println("מייל חוקי: " + emailMatcher.group());
        } else {
            System.out.println("מייל לא חוקי.");
        }
        
        // החלפת טקסט
        String text = "תפוחים 5, תפוזים 3, גזר 4";
        String replacedText = text.replaceAll("\d", "#");
        System.out.println(replacedText);  // "תפוחים #, תפוזים #, גזר #"
    }
}
```
פלט דוגמה:
```
מייל חוקי: example@domain.com
תפוחים #, תפוזים #, גזר #
```

## צלילה לעומק:
בשנת 1950, מתמטיקאי בשם סטיבן קליני המציא את מושג הביטויים הרגילים. כיום ישנם אלטרנטיבות כמו פרסרים של דקדוקים פורמליים, אך רגולר אקספרשנז עדיין נפוצים בשל פשטותם והיעילות שהם מציעים. בג'אווה, המחלקה `Pattern` מבצעת קומפילציה של הביטוי הרגיל והמחלקה `Matcher` משמשת לזיהוי התאמות ולביצוע פעולות עם הביטויים הרגילים.

## ראי גם:
- התיעוד הרשמי של ג'אווה לרגולר אקספרשנז: [Java Regex](https://docs.oracle.com/en/java/javase/16/docs/api/java.base/java/util/regex/Pattern.html)
- מדריך ויזואלי לרגולר אקספרשנז: [Regexr](https://regexr.com/)
- ספר מקוון חינמי על רגולר אקספרשנז: [RegexOne](https://regexone.com/)
