---
title:                "חילוץ תת-מחרוזות"
aliases:
- /he/java/extracting-substrings.md
date:                  2024-01-20T17:45:54.952332-07:00
model:                 gpt-4-1106-preview
simple_title:         "חילוץ תת-מחרוזות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## מה ולמה?
חילוץ תת-מחרוזות הוא התהליך שבו אנו קוטעים חלק ממחרוזת. מתכנתים עושים זאת כדי לפרק את הנתונים או לקבל את המידע הרלוונטי בלבד.

## איך לעשות:
הנה דוגמה פשוטה לחילוץ תת-מחרוזת ב-Java:

```java
public class SubstringExample {
    public static void main(String[] args) {
        String fullString = "שלום עולם!";
        String subString = fullString.substring(6, 10);

        System.out.println("תת-מחרוזת: " + subString);
    }
}
```

פלט:

```
תת-מחרוזת: עולם
```

## צלילה לעומק:
חילוץ תת-מחרוזות מתבצע ב-Java בעזרת המתודה `substring()`, שנמצאת במחלקה `String`. השיטה הזו נמצאת איתנו כמעט מראשית ימי ה-Java. יש גם אלטרנטיבות כמו רגקס (RegEx) או מחלקות כמו `StringBuilder`, אבל `substring()` נשארת הבחירה הפופולרית עקב פשטותה. ברמת המימוש, `substring()` ביצועית מאוד, כיוון שהיא מחזירה מחרוזת שמשתף את סדרת התווים של המחרוזת המקורית עד גרסאות מסוימות של Java, ובגרסאות החדשות יותר, יוצרת עותק חדש של סדרת התווים הנדרשת.

## ראו גם:
- [התיעוד הרשמי של `substring()` ב-Java](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html#substring(int,int))
- [מדריך ל-RegEx ב-Java](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)
- [תיעוד על StringBuilder](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/StringBuilder.html)
