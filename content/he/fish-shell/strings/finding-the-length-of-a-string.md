---
date: 2024-01-20 17:47:21.504480-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05DC\u05D3\u05D5\
  \u05D2\u05DE\u05D4, \u05D0\u05DD \u05D9\u05E9 \u05DC\u05DB\u05DD \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA \u05D5\u05D0\u05EA\u05DD \u05E8\u05D5\u05E6\u05D9\u05DD \u05DC\
  \u05D3\u05E2\u05EA \u05DB\u05DE\u05D4 \u05EA\u05D5\u05D5\u05D9\u05DD \u05D9\u05E9\
  \ \u05D1\u05D4, \u05EA\u05E9\u05EA\u05DE\u05E9\u05D5 \u05D1\u05E4\u05E7\u05D5\u05D3\
  \u05D4 `string length`."
lastmod: '2024-03-13T22:44:40.029945-06:00'
model: gpt-4-1106-preview
summary: "\u05DC\u05D3\u05D5\u05D2\u05DE\u05D4, \u05D0\u05DD \u05D9\u05E9 \u05DC\u05DB\
  \u05DD \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D5\u05D0\u05EA\u05DD \u05E8\u05D5\
  \u05E6\u05D9\u05DD \u05DC\u05D3\u05E2\u05EA \u05DB\u05DE\u05D4 \u05EA\u05D5\u05D5\
  \u05D9\u05DD \u05D9\u05E9 \u05D1\u05D4, \u05EA\u05E9\u05EA\u05DE\u05E9\u05D5 \u05D1\
  \u05E4\u05E7\u05D5\u05D3\u05D4 `string length`."
title: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA"
weight: 7
---

## איך לעשות:
לדוגמה, אם יש לכם מחרוזת ואתם רוצים לדעת כמה תווים יש בה, תשתמשו בפקודה `string length`.

```Fish Shell
set my_string "שלום עולם"
echo $my_string | string length
```

פלט:

```
10
```

הפקודה משיבה 10, שזה אורך השרשרת, כולל הרווח.

## טבילה עמוקה
מציאת אורך שרשרת היא פעולה בסיסית אבל קריטית בכל שפת תכנות. ב-Fish Shell, `string length` פועלת על שרשרות ישירות או קלטים מפקודות אחרות דרך pipeline (|). בניגוד לשפות אחרות שבהן אורך השרשרת נמצא בתוך המשתנה עצמו, ב-Fish אתם צריכים לשלוח את השרשרת לתכנית העוסקת באורך השרשרות, וזהו - פשוט וקל. ללא צורך בלולאות מסורבלות או בדיקות ידניות.

## ראה גם
- מדריך רשמי של Fish Shell על מניפולציות שרשרת: https://fishshell.com/docs/current/cmds/string.html
- מאמרים על Fish Scripting: https://fishshell.com/docs/current/tutorial.html
- פורום תמיכה של Fish: https://fishshell.com/docs/current/index.html#discussion
