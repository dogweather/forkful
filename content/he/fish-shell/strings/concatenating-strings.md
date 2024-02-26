---
date: 2024-01-20 17:34:40.435298-07:00
description: "\u05D1\u05DE\u05D9\u05DC\u05D9\u05DD \u05E4\u05E9\u05D5\u05D8\u05D5\u05EA\
  , \u05E9\u05E8\u05E9\u05D5\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D6\
  \u05D4 \u05D4\u05D3\u05D1\u05E7\u05D4 \u05E9\u05DC \u05E9\u05EA\u05D9 \u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05D5\u05EA \u05D0\u05D5 \u05D9\u05D5\u05EA\u05E8 \u05D9\u05D7\
  \u05D3. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\
  \u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D1\u05E0\u05D5\u05EA \u05DE\u05D9\
  \u05D3\u05E2 \u05DE\u05EA\u05D5\u05DA \u05D7\u05EA\u05D9\u05DB\u05D5\u05EA \u05D8\
  \u05E7\u05E1\u05D8 \u05E7\u05D8\u05E0\u05D5\u05EA."
lastmod: '2024-02-25T18:49:38.273989-07:00'
model: gpt-4-1106-preview
summary: "\u05D1\u05DE\u05D9\u05DC\u05D9\u05DD \u05E4\u05E9\u05D5\u05D8\u05D5\u05EA\
  , \u05E9\u05E8\u05E9\u05D5\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D6\
  \u05D4 \u05D4\u05D3\u05D1\u05E7\u05D4 \u05E9\u05DC \u05E9\u05EA\u05D9 \u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05D5\u05EA \u05D0\u05D5 \u05D9\u05D5\u05EA\u05E8 \u05D9\u05D7\
  \u05D3. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\
  \u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D1\u05E0\u05D5\u05EA \u05DE\u05D9\
  \u05D3\u05E2 \u05DE\u05EA\u05D5\u05DA \u05D7\u05EA\u05D9\u05DB\u05D5\u05EA \u05D8\
  \u05E7\u05E1\u05D8 \u05E7\u05D8\u05E0\u05D5\u05EA."
title: "\u05E9\u05E8\u05E9\u05D5\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
במילים פשוטות, שרשור מחרוזות זה הדבקה של שתי מחרוזות או יותר יחד. תכנתים עושים את זה כדי לבנות מידע מתוך חתיכות טקסט קטנות.

## איך לעשות:
ב-Fish Shell, שרשרת מחרוזות בקלות עם הסימן `.` בין המחרוזות, או פשוט על ידי השמטת רווחים:

```Fish Shell
# דוגמא 1 - עם נקודה
set greeting "שלום"
set name "עולם"
echo $greeting.$name  # יוצא "שלוםעולם"

# דוגמא 2 - בלי נקודה
echo $greeting$name  # גם יוצא "שלוםעולם"
```

הפלט של שתי הדוגמאות יהיה אותו הדבר. רק הדבק את המחרוזות ביחד והן יהפכו לאחת.

## צלילה עמוקה
פעם, בשפות קודמות, שרשור מחרוזות היה תהליך מסורבל. למשל, ב-C היה צורך בפונקציית `strcat`. Fish Shell הפשיט זאת - פשוט הדבק בין מחרוזות והן יחברו.

יש אלטרנטיבות, כמו השימוש בפקודת `string join` עם ללא רווח כמפריד:
```Fish Shell
echo (string join '' $greeting $name)  # כן, גם יוצא "שלוםעולם"
```

בימינו, שרשור מחרוזות ב-Fish הוא לא רק עניין של נוחות כתיבה, אלא גם שימושי לשילוב משתנים וטקסט בתסריטים.

## ראה גם
- [תיעוד רשמי של Fish Shell לפקודה `string`](https://fishshell.com/docs/current/cmds/string.html)
- [מדריך למתחילים ל-Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [תיעוד של שפת התכנות Fish](https://fishshell.com/docs/current/index.html)
