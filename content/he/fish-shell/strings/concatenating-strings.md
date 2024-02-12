---
title:                "שרשור מחרוזות"
aliases: - /he/fish-shell/concatenating-strings.md
date:                  2024-01-20T17:34:40.435298-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרשור מחרוזות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/concatenating-strings.md"
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
