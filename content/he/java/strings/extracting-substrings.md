---
date: 2024-01-20 17:45:54.952332-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D4\u05E0\u05D4\
  \ \u05D3\u05D5\u05D2\u05DE\u05D4 \u05E4\u05E9\u05D5\u05D8\u05D4 \u05DC\u05D7\u05D9\
  \u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D1-Java."
lastmod: '2024-03-13T22:44:39.112398-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05E0\u05D4 \u05D3\u05D5\u05D2\u05DE\u05D4 \u05E4\u05E9\u05D5\u05D8\
  \u05D4 \u05DC\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05EA \u05D1-Java."
title: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA"
weight: 6
---

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
