---
date: 2024-01-26 03:44:14.562607-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-Arduino, \u05EA\
  \u05D5\u05DB\u05DC\u05D5 \u05DC\u05E2\u05D2\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\
  \u05DD \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05E4\u05D5\u05E0\u05E7\u05E6\
  \u05D9\u05D5\u05EA \u05DE\u05D5\u05D1\u05E0\u05D5\u05EA. \u05D4\u05E9\u05D7\u05E7\
  \u05E0\u05D9\u05DD \u05D4\u05DE\u05E8\u05DB\u05D6\u05D9\u05D9\u05DD \u05D4\u05DD\
  \ `round`, `ceil` \u05D5-`floor`. \u05D4\u05E0\u05D4 \u05D4\u05D3\u05D2\u05DE\u05D4\
  \ \u05DE\u05D4\u05D9\u05E8\u05D4."
lastmod: '2024-03-13T22:44:39.758998-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-Arduino, \u05EA\u05D5\u05DB\u05DC\u05D5 \u05DC\u05E2\u05D2\u05DC\
  \ \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA\
  \ \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA \u05DE\u05D5\u05D1\u05E0\u05D5\
  \u05EA."
title: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD"
weight: 13
---

## איך לעשות:
ב-Arduino, תוכלו לעגל מספרים באמצעות פונקציות מובנות. השחקנים המרכזיים הם `round`, `ceil` ו-`floor`. הנה הדגמה מהירה:

```arduino
void setup() {
  Serial.begin(9600);
  
  float myNumber = 123.4567;

  // עיגול למספר השלם הקרוב ביותר
  Serial.println(round(myNumber)); // פלט: 123

  // תמיד מעגל למעלה
  Serial.println(ceil(myNumber));  // פלט: 124

  // תמיד מעגל למטה
  Serial.println(floor(myNumber)); // פלט: 123
}

void loop() {
  // אין מה לעבור בלולאה.
}
```

## צלילה עמוקה:
לאלגוריתמי עיגול יש היסטוריה ארוכה; הם קיימים הרבה לפני המחשבים הדיגיטליים. בחישוב אנלוגי, העיגול היה תהליך פיזי. בחישוב דיגיטלי, זהו תהליך מתמטי.

נדרש עיגול כאשר אנו ממירים מסוג עם דיוק גבוה יותר (כמו `float` או `double`) לסוג עם דיוק נמוך יותר (כמו `int`). אבל האופן שבו אנו מעגלים יכול להשתנות:

1. `round()`: עיגול סטנדרטי. אם השבר הוא 0.5 או גבוה יותר, הוא מעוגל למעלה; אחרת, הוא מעוגל למטה.
2. `ceil()`: קיצור של "תקרה", תמיד מעגל למעלה למספר השלם הקרוב ביותר, גם אם הוא קרוב יותר למספר הנמוך.
3. `floor()`: ההפך מתקרה; תמיד מעגל למטה.

הבחירה בין הפונקציות הללו תלויה במה שהערך המעוגל מיועד לו. מדידות עשויות להזדקק לעיגול סטנדרטי, כסף לעיתים קרובות משתמש ב-`floor`, בעוד שמערכות מלאי עשויות להשתמש ב-`ceil` כדי להבטיח שהכול נמנה.

היישום של פונקציות אלו ב-Arduino הוא פשוט; הן אינן מטפלות במקרים נוספים כמו עיגול למקומות עשרוניים ספציפיים. לשם כך, פונקציה מותאמת אישית או מתמטיקה מתקדמת צפויות להיכנס לתמונה - חשבו על הכפלה להזזת הנקודה העשרונית, עיגול, ולאחר מכן חלוקה בחזרה.

שגיאות עיגול יכולות להיצבר, ולהשפיע משמעותית על חישובים ארוכים או תהליכים איטרטיביים. מתכנתים צריכים להיות זהירים כאשר הם מבצעים מספר פעולות על ערכים מעוגלים.

## ראו גם:
2. מבט מעמיק על המלכודות והאסטרטגיות לעיגול: [מדריך הנקודה הצפה](https://floating-point-gui.de/)
3. לטכניקות מתקדמות, כולל פונקציות עיגול מותאמות אישית וטיפול בשגיאת עיגול, כדאי לבדוק מקורות אקדמיים או מדריכי תכנות מפורטים.
