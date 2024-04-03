---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:23.197392-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: C \u05D0\u05D9\u05E0\
  \u05D4 \u05DE\u05DB\u05D9\u05DC\u05D4 \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4\
  \ \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA \u05DC\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05D5\u05EA \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\
  \u05D8\u05E0\u05D5\u05EA \u05D9\u05E9\u05D9\u05E8\u05D5\u05EA, \u05D1\u05E0\u05D9\
  \u05D2\u05D5\u05D3 \u05DC\u05D7\u05DC\u05E7 \u05DE\u05D4\u05E9\u05E4\u05D5\u05EA\
  \ \u05D1\u05E8\u05DE\u05D4 \u05D2\u05D1\u05D5\u05D4\u05D4 \u05D9\u05D5\u05EA\u05E8\
  . \u05E2\u05DD \u05D6\u05D0\u05EA, \u05E0\u05D9\u05EA\u05DF \u05DC\u05D9\u05D9\u05E9\
  \u05DD \u05D0\u05EA \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05D1\u05E7\u05DC\u05D5\
  \u05EA \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA\u2026"
lastmod: '2024-03-13T22:44:40.100355-06:00'
model: gpt-4-0125-preview
summary: "C \u05D0\u05D9\u05E0\u05D4 \u05DE\u05DB\u05D9\u05DC\u05D4 \u05E4\u05D5\u05E0\
  \u05E7\u05E6\u05D9\u05D4 \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA \u05DC\u05D4\u05DE\
  \u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05DC\u05D0\u05D5\u05EA\
  \u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA \u05D9\u05E9\u05D9\u05E8\u05D5\
  \u05EA, \u05D1\u05E0\u05D9\u05D2\u05D5\u05D3 \u05DC\u05D7\u05DC\u05E7 \u05DE\u05D4\
  \u05E9\u05E4\u05D5\u05EA \u05D1\u05E8\u05DE\u05D4 \u05D2\u05D1\u05D5\u05D4\u05D4\
  \ \u05D9\u05D5\u05EA\u05E8."
title: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA"
weight: 4
---

## איך לעשות:
C אינה מכילה פונקציה מובנית להמרת מחרוזות לאותיות קטנות ישירות, בניגוד לחלק מהשפות ברמה גבוהה יותר. עם זאת, ניתן ליישם את התהליך בקלות באמצעות פונקציות של ספריית התקן עבור C. להלן מדריך צעד אחר צעד ודוגמה הממחישה איך להמיר מחרוזת לאותיות קטנות.

```c
#include <stdio.h>
#include <ctype.h>

void toLowerCase(char *str) {
    while (*str) {
        *str = tolower(*str);
        str++;
    }
}

int main() {
    char text[] = "Hello, World!";
    printf("Original: %s\n", text);

    toLowerCase(text);
    printf("Lowercase: %s\n", text);

    return 0;
}
```

**פלט לדוגמה:**

```
Original: Hello, World!
Lowercase: hello, world!
```

בדוגמה זו, הפונקציה `toLowerCase` עוברת על כל אחת מהתווים שבמחרוזת הקלט, וממירה אותם למקבילם הקטן באמצעות הפונקציה `tolower` מתוך `ctype.h`. השינוי נעשה במקום, ומשנה את המחרוזת המקורית.

## צלילה עמוקה
הפונקציה `tolower` המשמשת בדוגמה לעיל היא חלק מספריית התקן של C, בפרט בתוך קובץ הכותרת `ctype.h`. היא פועלת בהתאם לאזור המקומי הנוכחי, אך עבור האזור ה"סטנדרטי" של C, היא מטפלת בסט התווים של ASCII, שבו 'A' עד 'Z' מומרים ל-'a' עד 'z'.

בהיסטוריה, טיפול בקידוד תווים והמרת האותיות לקטנות / גדולות ב-C היה קשור הדוקות לסט התווים של ASCII, מה שהגביל את שימושיותו ביישומים בינלאומיים או מותאמים לוקאלית שבהם תווים מחוץ לסט ה-ASCII הם שכיחים. שפות תכנות מודרניות עשויות להציע שיטות מחרוזת מובנות לביצוע המרת מקרה בהתחשב באזור ובתווים של Unicode, שלא קיימות באופן טבעי ב-C.

בסצנריות שדורשות טיפול מרחיק לכת בטקסט, במיוחד עם תווים שאינם ASCII, מתכנתים עשויים לשקול שימוש בספריות המציעות תמיכה טובה יותר באינטרנשיונליזציה, כמו ICU (International Components for Unicode). עם זאת, לרוב היישומים העוסקים בטקסט ASCII, השיטה המוצגת היא יעילה ופשוטה. היא מדגישה את נטייתה של C לתת למתכנתים שליטה על טיפול בנתונים, אף על פי שזה דורש קצת יותר עבודה לעומת שפות ברמה גבוהה יותר.
